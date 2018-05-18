{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module InputSelection.Evaluation (
    evaluateInputPolicies
    -- * Interpreter
  , Stats(..)
  , IntState -- Opaque
  , initIntState
  , intPolicy
  ) where

import           Universum

import           Control.Lens ((%=), (+=), (.=), (<<+=))
import           Control.Lens.TH (makeLenses)
import           Control.Lens.Wrapped (_Wrapped)
import           Data.Conduit
import qualified Data.Conduit.Lift as Conduit
import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import           Formatting (build, sformat, (%))
import           Pos.Util.Chrono
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((<.>), (</>))
import qualified System.IO as IO
import           Text.Printf (printf)

import           InputSelection.Generator (Event (..))
import qualified InputSelection.Generator as Gen
import           InputSelection.Policy (InputSelectionPolicy, PrivacyMode (..), RunPolicy (..))
import qualified InputSelection.Policy as Policy
import           Util.Histogram (BinSize (..), Histogram, Range (..), Ranges (..))
import qualified Util.Histogram as Histogram
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Observarions we make at each step
-------------------------------------------------------------------------------}

-- | Observe the current state
data Observation h a = Observation {
      -- | Current UTxO
      obsUtxo      :: Utxo h a

      -- | Size of the current UTxO
    , obsUtxoSize  :: Int

      -- | Histogram of the current UTxO
    , obsHistogram :: Histogram
    }

mkObservation :: BinSize -> Utxo h a -> Observation h a
mkObservation binSize utxo = Observation {
      obsUtxo      = utxo
    , obsUtxoSize  = Map.size (utxoToMap utxo)
    , obsHistogram = utxoHistogram binSize utxo
    }

utxoHistogram :: BinSize -> Utxo h a -> Histogram
utxoHistogram binSize =
    Histogram.discretize binSize . map fromIntegral . outputs
  where
    outputs :: Utxo h a -> [Value]
    outputs = map (outVal . snd) . utxoToList

{-------------------------------------------------------------------------------
  Running statistics
-------------------------------------------------------------------------------}

-- | Accumulated statistics
data Stats = Stats {
      -- | Frame
      --
      -- This is just a simple counter
      _statsFrame          :: !Int

      -- | Maximum values across all bins
    , _statsMaxHistogram   :: !Histogram

      -- | Number of payment requests we failed to satisfy
    , _statsFailedPayments :: !Int

      -- | Size of the UTxO over time
    , _statsUtxoGrowth     :: !(NewestFirst [] Int)

      -- | Histogram of number of inputs
      --
      -- Each bin @n@ (of size 1) corresponds to @n@ inputs; the count
      -- tells us how many transactions have exactly @n@ inputs.
    , _statsTxInputs       :: !Histogram
    }

makeLenses ''Stats

initStats :: BinSize -> Stats
initStats binSize = Stats {
      _statsFrame          = 0
    , _statsMaxHistogram   = Histogram.empty binSize
    , _statsFailedPayments = 0
    , _statsUtxoGrowth     = NewestFirst []
    , _statsTxInputs       = Histogram.empty (BinSize 1)
    }

-- | Update statistics with most recent observation
updateStats :: Observation h a -> Stats -> Stats
updateStats Observation{..} st =
    st & statsFrame                 %~ succ
       & statsMaxHistogram          %~ Histogram.max obsHistogram
       & statsUtxoGrowth . _Wrapped %~ (obsUtxoSize :)

{-------------------------------------------------------------------------------
  Interpreter state
-------------------------------------------------------------------------------}

data IntState h a = IntState {
      _stUtxo       :: Utxo h a
    , _stPending    :: Utxo h a
    , _stStats      :: Stats
    , _stFreshHash  :: Int

      -- | Change address
      --
      -- NOTE: At the moment we never modify this; we're not evaluating
      -- privacy, so change to a single address is fine.
    , _stChangeAddr :: a

      -- | Binsize used for histograms
      --
      -- We cannot actually currently change this as we run the interpreter
      -- because `Histogram.max` only applies to histograms wit equal binsizes.
    , _stBinSize    :: BinSize
    }

makeLenses ''IntState

initIntState :: BinSize -> Utxo h a -> a -> IntState h a
initIntState binSize utxo changeAddr = IntState {
      _stUtxo       = utxo
    , _stPending    = utxoEmpty
    , _stStats      = initStats binSize
    , _stFreshHash  = 1
    , _stChangeAddr = changeAddr
    , _stBinSize    = binSize
    }

instance Monad m => RunPolicy (StateT (IntState h a) m) a where
  genChangeAddr = use stChangeAddr
  genFreshHash  = stFreshHash <<+= 1

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

-- | Construct an observation and update the statistics
observe :: Monad m => StateT (IntState h a) m (Observation h a, Stats)
observe = state aux
  where
    aux :: IntState h a -> ((Observation h a, Stats), IntState h a)
    aux st = ((obs, stats), st & stStats .~ stats)
      where
        obs   = mkObservation (st ^. stBinSize) (st ^. stUtxo)
        stats = updateStats obs (st ^. stStats)

-- | Interpreter for events, evaluating a policy
--
-- Turns a stream of events into a stream of observations and accumulated
-- statistics.
--
-- Returns the final state
intPolicy :: forall h a m. (Hash h a, Monad m)
          => InputSelectionPolicy h a (StateT (IntState h a) m)
          -> IntState h a -- Initial state
          -> ConduitT (Event h a) (Observation h a, Stats) m (IntState h a)
intPolicy policy initState =
    Conduit.execStateC initState $
      awaitForever $ \event -> do
        lift $ go event
        yield =<< lift observe
  where
    go :: Event h a -> StateT (IntState h a) m ()
    go (Deposit new) =
        stUtxo %= utxoUnion new
    go NextSlot = do
        -- TODO: May want to commit only part of the pending transactions
        pending <- use stPending
        stUtxo    %= utxoUnion pending
        stPending .= utxoEmpty
    go (Pay outs) = do
        utxo <- use stUtxo
        mtx  <- policy utxo outs
        case mtx of
          Right tx -> do
            stUtxo                  %= utxoRemoveInputs (trIns tx)
            stPending               %= utxoUnion (trUtxo tx)
            stStats . statsTxInputs %= Histogram.add (length (trIns tx)) 1
          Left _err ->
            stStats . statsFailedPayments += 1

{-------------------------------------------------------------------------------
  Gnuplot output
-------------------------------------------------------------------------------}

-- | Plot instructions
--
-- As we render the observations, we collect a bunch of plot instructions.
-- The reason that we do not execute these as we go is that we do not know
-- a priori which ranges we should use for the graphs (and it is important
-- that we use the same range for all frames).
data PlotInstr = PlotInstr {
      -- | Filename of the frame
      piFrame          :: FilePath

      -- | Number of failed payment attempts
    , piFailedPayments :: Int
    }

-- | Frame bounds
--
-- When we render all frames, they should also use the same bounds for the
-- animation to make any sense. This is also useful to have animations of
-- _different_ policies use the same bounds.
data Bounds = Bounds {
      -- | Binsize used for the utxo
      boundsBinSize  :: BinSize

      -- | Range of the UTxO
    , boundsUtxo     :: Ranges

      -- | Range of the UTxO size
    , boundsGrowth   :: Ranges

      -- | Range of the transaction inputs
    , boundsTxInputs :: Ranges
    }

-- | Derive compute final bounds from final state
deriveBounds :: IntState h a -> Bounds
deriveBounds st = Bounds {
      boundsBinSize  = st ^. stBinSize
    , boundsUtxo     = Histogram.range (stats ^. statsMaxHistogram)
    , boundsGrowth   = Ranges {
                           xRange = Range 0 (stats ^. statsFrame)
                         , yRange = Range 0 (maximum (stats ^. statsUtxoGrowth))
                         }
    , boundsTxInputs = Histogram.range (stats ^. statsTxInputs)
    }
  where
    stats = st ^. stStats

-- | Take the maximum bounds
--
-- This is very useful when rendering two plots to the same bounds.
maxBounds :: Bounds -> Bounds -> Bounds
maxBounds a b =
    if boundsBinSize a /= boundsBinSize b
      then error "maxBounds: different bin size"
      else Bounds {
          boundsBinSize  = boundsBinSize a
        , boundsUtxo     = maxUsing Histogram.unionRanges boundsUtxo
        , boundsGrowth   = maxUsing Histogram.unionRanges boundsGrowth
        , boundsTxInputs = maxUsing Histogram.unionRanges boundsTxInputs
        }
  where
    maxUsing :: (a -> a -> a) -> (Bounds -> a) -> a
    maxUsing op f = f a `op` f b

-- | Render in gnuplot syntax
renderPlotInstr :: Bounds -> PlotInstr -> Text
renderPlotInstr Bounds{..} PlotInstr{..} = sformat
    ( "# Frame " % build % "\n"
    % "set output '" % build % ".png'\n"
    % "set multiplot\n"

    % "set xrange " % build % "\n"
    % "set yrange " % build % "\n"
    % "set size 0.7,1\n"
    % "set origin 0,0\n"
    % "set xtics autofreq\n"
    % "set label 1 'failed: " % build % "' at graph 0.95, 0.90 front right\n"
    % "set boxwidth " % build % "\n"
    % "plot '" % build % ".histogram' using 1:2 with boxes\n"
    % "unset label 1\n"

    % "set xrange " % build % "\n"
    % "set yrange " % build % "\n"
    % "set size 0.25,0.4\n"
    % "set origin 0.05,0.55\n"
    % "unset xtics\n"
    % "plot '" % build % ".growth' notitle\n"

    % "set xrange " % build % "\n"
    % "set yrange " % build % "\n"
    % "set size 0.25,0.4\n"
    % "set origin 0.65,0.55\n"
    % "set xtics 1\n"
    % "set boxwidth 1\n"
    % "plot '" % build % ".txinputs' using 1:2 with boxes fillstyle solid notitle\n"

    % "unset multiplot\n"
    )

    piFrame
    piFrame

    (xRange boundsUtxo)
    (yRange boundsUtxo)
    piFailedPayments
    boundsBinSize
    piFrame

    (xRange boundsGrowth)
    (yRange boundsGrowth)
    piFrame

    (xRange boundsTxInputs)
    (yRange boundsTxInputs)
    piFrame

-- | Render a complete set of plot instructions
writePlotInstrs :: FilePath -> Bounds -> [PlotInstr] -> IO ()
writePlotInstrs fp bounds is = do
    putStrLn $ sformat ("Writing '" % build % "'") fp
    withFile fp WriteMode $ \h -> do
      Text.hPutStrLn h $ sformat
          ( "set grid\n"
          % "set term png size 800, 400\n"
          )
      forM_ is $ Text.hPutStrLn h . renderPlotInstr bounds

{-------------------------------------------------------------------------------
  Render observations
-------------------------------------------------------------------------------}

-- | Sink that writes observations to disk
writeObservations :: forall h a m. MonadIO m
                  => FilePath -- ^ Prefix for the files to create
                  -> ConduitT (Observation h a, Stats) Void m [PlotInstr]
writeObservations prefix =
    loop []
  where
    loop :: [PlotInstr] -> ConduitT (Observation h a, Stats) Void m [PlotInstr]
    loop acc = do
        mObs <- await
        case mObs of
          Nothing  -> return $ reverse acc
          Just obs -> loop . (: acc) =<< liftIO (go obs)

    go :: (Observation h a, Stats) -> IO PlotInstr
    go (Observation{..}, stats) = do
        Histogram.writeFile (filepath <.> "histogram") obsHistogram
        Histogram.writeFile (filepath <.> "txinputs") (stats ^. statsTxInputs)
        writeGrowth (filepath <.> "growth") (stats ^. statsUtxoGrowth)
        return PlotInstr {
            piFrame          = filename
          , piFailedPayments = stats ^. statsFailedPayments
          }
      where
        filename = printf "%08d" (stats ^. statsFrame)
        filepath = prefix </> filename

    writeGrowth :: FilePath -> NewestFirst [] Int -> IO ()
    writeGrowth fp (NewestFirst ss) =
        withFile fp WriteMode $ \h ->
          forM_ (reverse ss) $
            IO.hPrint h

{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

-- | Evaluate a policy
--
-- Returns the bounds and plot instructions; we return these separately so that
-- we combine bounds of related plots and draw them with the same scales.
evaluatePolicy :: Hash h a
               => FilePath
               -> InputSelectionPolicy h a (StateT (IntState h a) IO)
               -> IntState h a
               -> ConduitT () (Event h a) IO ()
               -> IO (Bounds, [PlotInstr])
evaluatePolicy prefix policy initState generator = do
    createDirectoryIfMissing False prefix
    fmap (first deriveBounds) $
      runConduit $
        generator                  `fuse`
        intPolicy policy initState `fuseBoth`
        writeObservations prefix

evaluateInputPolicies :: FilePath -> IO ()
evaluateInputPolicies prefix = do
    (exactBounds, exactPlot) <- evaluatePolicy
      (prefix </> "exact")
      Policy.exactSingleMatchOnly
      (initIntState (BinSize 10) utxoEmpty ())
      (Gen.test Gen.defTestParams)
    writePlotInstrs
      (prefix </> "exact" </> "mkframes.gnuplot")
      exactBounds
      exactPlot

    (trivialOffBounds, trivialOffPlot) <- evaluatePolicy
      (prefix </> "trivialOff")
      (Policy.random PrivacyModeOff)
      (initIntState (BinSize 10) utxoEmpty ())
      (Gen.trivial 1000 100 1000)
    (trivialOnBounds, trivialOnPlot) <- evaluatePolicy
      (prefix </> "trivialOn")
      (Policy.random PrivacyModeOn)
      (initIntState (BinSize 10) utxoEmpty ())
      (Gen.trivial 1000 100 1000)
    let trivialBounds = maxBounds trivialOffBounds trivialOnBounds
    writePlotInstrs
      (prefix </> "trivialOff" </> "mkframes.gnuplot")
      trivialBounds
      trivialOffPlot
    writePlotInstrs
      (prefix </> "trivialOn" </> "mkframes.gnuplot")
      trivialBounds
      trivialOnPlot


{-

input selection
coin selection

bitcoinj coin selection? ("multiple classes" -- multiple policies)

https://github.com/bitcoin/bitcoin/issues/7664

See ApproximateBestSubset in wallet.cpp.

sweep?



-}


{-
http://murch.one/wp-content/uploads/2016/11/erhardt2016coinselection.pdf
"An Evaluation of Coin Selection Strategies", Master’s Thesis, Mark Erhardt

2.3.4
A transaction output is labeled as dust when its value is similar to the cost of
spending it.Precisely, Bitcoin Core sets the dust limit to a value where spending an
2.3. Transactions 7
output would exceed 1/3 of its value. T

https://youtu.be/vnHQwYxB08Y?t=39m


https://www.youtube.com/watch?v=IKSSWUBqMCM

companies; hundreds of entries in UTxO
individuals: tens of entries

batch payments!
  (Harding, BitcoinTechTalk -- safe up to 80% of transaction fees)

coin selection --
  relatively minor importance
    batching, better representation (SegWit), .. much larger impact
    coin selection only a few percent savings

* FIFO is actually a reasonable strategy (!)
* So is random
    self correcting -- if you have a large amount of small inputs,
    they'll be more likely to be picked!
    (i.e, if 90% of the wallet is small inputs, 90% change of picking them!)

Branch&Bound seems to do exhaustive search (backtracking algorithm) to find
exact matches, coupled with random selection.

A Traceability Analysis of Monero’s Blockchain
https://eprint.iacr.org/2017/338.pdf
-}
