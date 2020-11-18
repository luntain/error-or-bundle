{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Time
  ( MonadTime (..),
  )
where

import Control.Monad.Trans
import Data.Time hiding (getCurrentTime)

-- | A class interface with overridable time implementation.
-- Import `Control.Monad.Time.DefaultInstance` for the default
-- implementation using the real underlying time functions.
-- See `Control.Monad.Trans.SimulatedTime` for an instance
-- that allows to control time.
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
