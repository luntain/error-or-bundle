{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-} -- needed for PrimMonad in GHC version prior to 9.4
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | This module provides a monad transformer that implements
-- `Control.Monad.Time.MonadTime` and allows to control time in tests.
module Control.Monad.Trans.SimulatedTime
  ( SimulatedTimeT (..),
    runSimulatedTimeT,
    getTimeEnv,
    module Test.SimulatedTime
  )
where

import Control.Applicative (Alternative)
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.RWS (MonadState, MonadWriter)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Time
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Zip (MonadZip)
import Control.Monad.IO.Unlift
import Test.SimulatedTime

#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail (MonadFail(..))
#endif

-- | A newtype wrapper over `ReaderT` `TimeEnv`
newtype SimulatedTimeT m a = SimulatedTimeT {unSimulatedTimeT :: ReaderT TimeEnv m a}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadIO,
      MonadState s,
      MonadWriter w,
      MonadTrans,
      MonadFail,
      MonadThrow,
      MonadCatch,
      MonadError e,
      MonadCont,
      MonadPlus,
      MonadFix,
      -- MonadUnliftIO, -- gives an incomprehensible typing error
      MonadResource,
      MonadZip
#if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
      , PrimMonad
#endif
    )

-- | Run the reader
runSimulatedTimeT :: SimulatedTimeT m a -> TimeEnv -> m a
runSimulatedTimeT = runReaderT . unSimulatedTimeT

instance MonadReader r m => MonadReader r (SimulatedTimeT m) where
  ask = lift ask
  local f = SimulatedTimeT . mapReaderT (local f) . unSimulatedTimeT
  reader = lift . reader

instance MonadIO m => MonadTime (SimulatedTimeT m) where
  getCurrentTime = SimulatedTimeT $ do
    env <- ask
    liftIO $ getSimulatedTime env

  threadDelay delay = SimulatedTimeT $ do
    env <- ask
    liftIO $ threadDelay' env delay

-- | Use 'TimeEnv' to control time in tests. For example calling
-- 'Time.SimlatedTime.advance' moves you to the future.
getTimeEnv :: Monad m => SimulatedTimeT m TimeEnv
getTimeEnv = SimulatedTimeT ask

instance MonadUnliftIO m => MonadUnliftIO (SimulatedTimeT m) where
   withRunInIO (inner :: (forall a. SimulatedTimeT m a -> IO a) -> IO b) =
     SimulatedTimeT (withRunInIO (\x -> inner (x . unSimulatedTimeT))) -- \x is needed to avoid some typing problems

instance MonadMask m => MonadMask (SimulatedTimeT m) where
    mask inner = SimulatedTimeT $ mask (\unmask -> unSimulatedTimeT $ inner (SimulatedTimeT . unmask . unSimulatedTimeT))
    uninterruptibleMask inner = SimulatedTimeT (uninterruptibleMask (\unmask -> unSimulatedTimeT $ inner (SimulatedTimeT . unmask . unSimulatedTimeT)))
    generalBracket acquire release inner =
      SimulatedTimeT $ generalBracket (unSimulatedTimeT acquire) (fmap (fmap unSimulatedTimeT) release) (fmap (unSimulatedTimeT) inner)
