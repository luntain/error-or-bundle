{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | The instance of 'MonadTime' for IO, using the underlying system time and does
-- This is to be used in production code, or in thoses tests where controling time
-- is not needed.
--
-- This is only and instance for IO, for instances for monad transformers see
-- 'Control.Monad.Trans.SimulatedTime'.
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
