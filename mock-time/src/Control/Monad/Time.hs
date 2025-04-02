{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Time
  ( MonadTime (..),
  )
where

import Control.Monad.Trans
import Data.Time hiding (getCurrentTime)

-- | A class abstracting the implementation of time in Haskell.
-- This module provides an overlappable default instance for monads
-- implementing MonadIO.
--
-- See `Control.Monad.Trans.SimulatedTime` for an instance
-- that allows to control time in tests.
class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime
  threadDelay :: Int -> m ()

instance {-# OVERLAPPABLE #-}
  ( MonadTime m,
    MonadTrans t,
    Monad (t m)
  ) =>
  MonadTime (t m)
  where
  getCurrentTime = lift getCurrentTime
  threadDelay a = lift (threadDelay a)

instance {-# OVERLAPPABLE #-}
  (Monad m, MonadIO  m) => MonadTime m where
  getCurrentTime = liftIO getCurrentTime
  threadDelay a = liftIO (threadDelay a)
