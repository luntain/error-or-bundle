{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Time
  ( MonadTime (..),
  )
where

import Control.Monad.Trans
import Data.Time hiding (getCurrentTime)

-- | An interface for time related functions
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
