{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | An implementation of `Control.Monad.Time.MonadTime` with a monad transformer.
module Control.Monad.Trans.SimulatedTime
  ( SimulatedTimeT (..),
    runSimulatedTimeT,
    RealTimeT(..),
    getTimeEnv,
    module Test.SimulatedTime
  )
where

import Control.Applicative (Alternative)
import qualified Control.Concurrent
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.RWS (MonadState, MonadWriter)
import Control.Monad.Reader
import Control.Monad.Time
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Zip (MonadZip)
import qualified Data.Time
import Control.Monad.IO.Unlift
import Test.SimulatedTime

#if __GLASGOW_HASKELL__ < 880
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
      MonadZip,
      PrimMonad
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

-- | Use 'TimeEnv' to control the simulated time, for instance call
-- 'Time.SimlatedTime.advance' on it to advance in simulated time.
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

-- | An implementation for `MonadTime` that uses real time directly,
-- not allowing for control. Use it from code where you need to choose
-- implementation for MockTime, but don't need to control the time.
--
-- Example: use this one in the executable, while `SimulatedTimeT` is
-- used in tests that need to control time.
newtype RealTimeT m a = RealTimeT {runRealTimeT :: m a}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadIO,
      MonadReader s,
      MonadState s,
      MonadWriter w,
      --MonadTrans, "cannot eta-reduce the representation type enough", says ghc
      MonadFail,
      MonadThrow,
      MonadCatch,
      MonadError e,
      MonadCont,
      MonadPlus,
      MonadFix,
      MonadResource,
      MonadZip,
      PrimMonad
    )

instance MonadIO m => MonadTime (RealTimeT m) where
  getCurrentTime = RealTimeT $ liftIO $ Data.Time.getCurrentTime
  threadDelay delay = RealTimeT $ do liftIO $ Control.Concurrent.threadDelay delay

instance MonadUnliftIO m => MonadUnliftIO (RealTimeT m) where
   withRunInIO (inner :: (forall a. RealTimeT m a -> IO a) -> IO b) =
     RealTimeT (withRunInIO (\x -> inner (x . runRealTimeT))) -- \x is needed to avoid some typing problems

instance MonadMask m => MonadMask (RealTimeT m) where
    mask inner = RealTimeT $ mask (\unmask -> runRealTimeT $ inner (RealTimeT . unmask . runRealTimeT))
    uninterruptibleMask inner = RealTimeT (uninterruptibleMask (\unmask -> runRealTimeT $ inner (RealTimeT . unmask . runRealTimeT)))
    generalBracket acquire release inner =
      RealTimeT $ generalBracket (runRealTimeT acquire) (fmap (fmap runRealTimeT) release) (fmap (runRealTimeT) inner)
