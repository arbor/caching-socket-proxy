{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module App.Application where

import App.AppEnv
import App.Orphans                  ()
import Arbor.Logger
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger         (LoggingT, MonadLogger)
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Text                    (Text)
import Network.AWS                  as AWS hiding (LogLevel)
import Network.StatsD               as S

type AppName = Text

newtype Application a = Application
  { unApp :: ReaderT AppEnv (LoggingT AWS) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadBase IO
             , MonadThrow
             , MonadCatch
             , MonadReader AppEnv
             , MonadAWS
             , MonadLogger
             , MonadResource)

class ( MonadReader AppEnv m
      , MonadLogger m
      , MonadAWS m
      , MonadStats m
      , MonadResource m
      , MonadThrow m
      , MonadCatch m
      , MonadIO m) => MonadApp m where

deriving instance MonadApp Application

instance MonadStats Application where
  getStatsClient = reader _appStatsClient

runApplicationM :: AppEnv
                -> Application ()
                -> IO ()
runApplicationM envApp f =
  runResourceT
    . runAWS (envApp ^. appAwsEnv)
    . runTimedLogT (envApp ^. appLogger . lgLogLevel) (envApp ^. appLogger . lgLogger)
    $ do
        logInfo $ show (envApp ^. appOptions)
        runReaderT (unApp f) envApp
