{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Histograms
--
-- Indended for qualified import.
--
-- > import Util.Histogram (Histogram)
-- > import qualified Util.Histogram as Histogram
module Util.Histogram (
    -- * Basic definitions
    Bin
  , Count
  , Histogram
    -- * Output
  , writeFile
    -- * Construction
  , BinSize(..)
  , discretize
  , empty
  , max
    -- * Updates
  , add
    -- * Gnuplot support
  , Range(..)
  , Ranges(..)
  , unionRange
  , unionRanges
  , range
  ) where

import           Universum hiding (empty, max, writeFile)
import qualified Universum

import qualified Data.Map as Map
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import qualified System.IO as IO

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

type Bin       = Int
type Count     = Int

data Histogram = Histogram {
      histogramBinSize :: BinSize
    , histogramToMap   :: Map Bin Count
    }

histogramToList :: Histogram -> [(Bin, Count)]
histogramToList = Map.toList . histogramToMap

{-------------------------------------------------------------------------------
  Output
-------------------------------------------------------------------------------}

-- | Write out histogram
--
-- Example plot using gnuplot:
--
-- > set grid
-- > set xrange [5:105]  -- for bins 10, 20, .. 100
-- > set yrange [0:10.5] -- for counts from 0 .. 10
-- > plot 'data.csv' using 1:2 with boxes
--
-- See 'range' to compute @xrange@ and @yrange@.
writeFile :: FilePath -> Histogram -> IO ()
writeFile fp hist =
    withFile fp WriteMode $ \h ->
      forM_ (histogramToList hist) $ \(step, count) ->
        IO.hPutStrLn h $ show step ++ "\t" ++ show count

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

newtype BinSize = BinSize Int
  deriving (Eq, Buildable)

-- | Construct histogram by counting all the doubles per bin
discretize :: BinSize -> [Double] -> Histogram
discretize (BinSize binSize) =
    Histogram (BinSize binSize) . go Map.empty
  where
    go :: Map Bin Count -> [Double] -> Map Bin Count
    go acc []     = acc
    go acc (d:ds) = let bin = floor (d / fromIntegral binSize) * binSize
                    in go (Map.alter incr bin acc) ds

    incr :: Maybe Count -> Maybe Count
    incr Nothing  = Just 1
    incr (Just n) = Just (n + 1)

-- | Empty histogram
empty :: BinSize -> Histogram
empty binSize = Histogram binSize Map.empty

-- | Takes maximum across all bins
--
-- This only makes sense if the two histograms uses the same bin size.
max :: Histogram -> Histogram -> Histogram
max a b =
    if histogramBinSize a /= histogramBinSize b
      then error "Cannot mappend two histograms with different bin sizes"
      else Histogram {
               histogramBinSize = histogramBinSize a
             , histogramToMap   = Map.unionWith Universum.max
                                    (histogramToMap a)
                                    (histogramToMap b)
             }

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

add :: Bin -> Count -> Histogram -> Histogram
add bin count (Histogram bz h) = Histogram bz (Map.alter aux bin h)
  where
    aux :: Maybe Count -> Maybe Count
    aux Nothing       = Just count
    aux (Just count') = Just (count + count')

{-------------------------------------------------------------------------------
  Gnuplot support
-------------------------------------------------------------------------------}

-- | Range with a 'Buildable' instance that produces valid gnuplot output
data Range = Range { rangeLo :: Int , rangeHi :: Int }

-- | X-range and y-range
data Ranges = Ranges { xRange :: Range, yRange :: Range }

-- | Union two 'Range's
unionRange :: Range -> Range -> Range
unionRange a b = Range {
      rangeLo = Universum.min (rangeLo a) (rangeLo b)
    , rangeHi = Universum.max (rangeHi a) (rangeHi b)
    }

-- | Union two 'Ranges's
unionRanges :: Ranges -> Ranges -> Ranges
unionRanges a b = Ranges {
      xRange = unionRange (xRange a) (xRange b)
    , yRange = unionRange (yRange a) (yRange b)
    }

instance Buildable Range where
  build (Range lo hi) = bprint ("[" % build % ":" % build % "]") lo hi

-- | X-range (bins) and Y-range (counts) for the given histogram
--
-- Calls 'error' if the histogram is empty.
range :: Histogram -> Ranges
range hist = Ranges {
      xRange = Range xLo xHi
    , yRange = Range yLo yHi
    }
  where
    histMap = histogramToMap hist
    BinSize binSize = histogramBinSize hist

    xLo = fst (Map.findMin histMap) - (binSize `div` 2)
    xHi = fst (Map.findMax histMap) + (binSize `div` 2)
    yLo = 0
    yHi = maximum histMap + 1
