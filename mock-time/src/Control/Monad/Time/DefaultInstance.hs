{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The default instance for 'MonadTime', using only the underlying system time.
-- This is to be used in production code, or in thoses tests where controling time
-- is not needed.
module Control.Monad.Time.DefaultInstance (
module Control.Monad.Time.DefaultInstance,
module Control.Monad.Time) where

import Control.Monad.Time
import Control.Monad.Trans (liftIO)

import Data.Time (getCurrentTime)
import Control.Concurrent (threadDelay)

instance {-# OVERLAPPING #-}
  MonadTime IO where
  getCurrentTime = liftIO Data.Time.getCurrentTime
  threadDelay delay = liftIO $ Control.Concurrent.threadDelay delay
