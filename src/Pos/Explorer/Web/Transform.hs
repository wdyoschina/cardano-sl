{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Explorer.Web.Transform
       ( ExplorerProd
       , explorerServeWebReal
       , explorerPlugin
       , notifierPlugin
       ) where

import           Universum

import qualified Control.Monad.Catch     as Catch (Handler (..), catches)
import           Control.Monad.Except    (MonadError (throwError))
import qualified Control.Monad.Reader    as Mtl
import           Mockable                (runProduction)
import           Servant.Server          (Handler)
import           Servant.Utils.Enter     ((:~>) (..), enter)

import           Pos.Communication       (OutSpecs, SendActions, WorkerSpec, worker)
import           Pos.Ssc.GodTossing      (SscGodTossing)
import           Pos.WorkMode            (RealMode, RealModeContext (..))

import           Pos.Explorer            (ExplorerBListener, runExplorerBListener)
import           Pos.Explorer.Socket.App (NotifierSettings, notifierApp)
import           Pos.Explorer.Web.Server (explorerApp, explorerHandlers,
                                          explorerServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type ExplorerProd = ExplorerBListener (RealMode SscGodTossing)

notifierPlugin :: NotifierSettings -> ([WorkerSpec ExplorerProd], OutSpecs)
notifierPlugin = first pure . worker mempty .
    \settings _sa -> notifierApp @SscGodTossing settings

explorerPlugin
    :: Word16
    -> FilePath -> FilePath -> FilePath
    -> ([WorkerSpec ExplorerProd], OutSpecs)
explorerPlugin port walletTLSCert walletTLSKey walletTLSca = first pure $ worker mempty $
    (\sa -> explorerServeWebReal sa port walletTLSCert walletTLSKey walletTLSca)

explorerServeWebReal
    :: SendActions ExplorerProd
    -> Word16
    -> FilePath -> FilePath -> FilePath
    -> ExplorerProd ()
explorerServeWebReal sendActions = explorerServeImpl
    (explorerApp $ flip enter (explorerHandlers sendActions) <$> nat)

nat :: ExplorerProd (ExplorerProd :~> Handler)
nat = do
    rctx <- ask
    pure $ NT (convertHandler rctx)

convertHandler
    :: RealModeContext SscGodTossing
    -> ExplorerProd a
    -> Handler a
convertHandler rctx handler =
    liftIO (realRunner $ runExplorerBListener $ handler) `Catch.catches` excHandlers
  where

    realRunner :: forall t . RealMode SscGodTossing t -> IO t
    realRunner act = runProduction $ Mtl.runReaderT act rctx

    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
