{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Time.DefaultInstance (
module Control.Monad.Time.DefaultInstance,
module Control.Monad.Time) where

import Control.Monad.Time
import Control.Monad.Trans (liftIO, MonadIO)

import Data.Time (getCurrentTime)
import Control.Concurrent (threadDelay)

instance {-# OVERLAPPABLE #-}
  (Monad m, MonadIO m) => MonadTime m where
  getCurrentTime = liftIO Data.Time.getCurrentTime
  threadDelay delay = liftIO $ Control.Concurrent.threadDelay delay
