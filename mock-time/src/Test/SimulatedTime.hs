{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.SimulatedTime
  ( SimulatedTimeT (..),
    runSimulatedTimeT,
    TimeEnv,
    create,
    advance,
    triggerEvents,
  )
where

import Control.Applicative (Alternative)
import Control.Concurrent hiding (threadDelay)
import qualified Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.RWS (MonadState, MonadWriter)
import Control.Monad.Reader
import Control.Monad.Time
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Zip (MonadZip)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (forM_)
import Data.Function (on)
import Data.Functor (void)
import Data.List (insertBy, partition)
import Data.Maybe (listToMaybe)
import Data.Time hiding (getCurrentTime)
import qualified Data.Time

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
      MonadError e,
      MonadCont,
      MonadPlus,
      MonadFix,
      MonadResource,
      MonadZip,
      PrimMonad
    )

runSimulatedTimeT :: SimulatedTimeT m a -> TimeEnv -> m a
runSimulatedTimeT = runReaderT . unSimulatedTimeT

instance MonadReader r m => MonadReader r (SimulatedTimeT m) where
  ask = lift ask
  local f = SimulatedTimeT . mapReaderT (local f) . unSimulatedTimeT
  reader = lift . reader

data TimeEnv
  = TimeEnv
      { offset :: TVar NominalDiffTime,
        events :: TVar [(UTCTime, MVar ())]
      }

instance MonadIO m => MonadTime (SimulatedTimeT m) where
  getCurrentTime = SimulatedTimeT $ do
    env <- ask
    liftIO $ getCurrentTime' env

  threadDelay delay = SimulatedTimeT $ do
    env <- ask
    liftIO $ threadDelay' env delay

create :: UTCTime -> IO TimeEnv
create epoch = do
  now <- Data.Time.getCurrentTime
  TimeEnv <$> newTVarIO (diffUTCTime epoch now) <*> newTVarIO []

getCurrentTime' :: TimeEnv -> IO UTCTime
getCurrentTime' t = do
  now <- Data.Time.getCurrentTime
  offset <- readTVarIO (offset t)
  return $ addUTCTime offset now

triggerEvents t = advance t 0

advance :: TimeEnv -> NominalDiffTime -> IO ()
advance t diff = do
  now <- Data.Time.getCurrentTime
  due <- atomically $ do
    unless (diff == 0) $ modifyTVar (offset t) (+ diff)
    modifyTVar (events t) (map (first (addUTCTime (- diff))))
    extractDueEvents t now
  forM_ due (flip putMVar ())
  scheduleNextWakeUp t now

-- the 'now' is the real time now, as opposed to simulated now
extractDueEvents :: TimeEnv -> UTCTime -> STM [MVar ()]
extractDueEvents t now = do
  (due, inTheFuture) <- partition ((<= now) . fst) <$> readTVar (events t)
  writeTVar (events t) inTheFuture
  return (map snd due)

scheduleNextWakeUp :: TimeEnv -> UTCTime -> IO ()
scheduleNextWakeUp t now = do
  nextEvent <- listToMaybe . map fst <$> readTVarIO (events t)
  case nextEvent of
    Nothing -> return ()
    Just tm -> do
      let us = ceiling (diffUTCTime tm now * 1000000)
      void (forkIO (threadDelay us >> triggerEvents t))

threadDelay' :: TimeEnv -> Int -> IO ()
threadDelay' t us = do
  mvar <- newEmptyMVar
  now <- Data.Time.getCurrentTime
  shouldScheduleWakeUp <- atomically $ do
    nextWakeUp <- listToMaybe . map fst <$> readTVar (events t)
    let eventTime = addUTCTime (fromIntegral us / (1000 * 1000)) now
    modifyTVar (events t) (insertBy (compare `on` fst) (eventTime, mvar))
    return (maybe True (eventTime <) nextWakeUp)
  when shouldScheduleWakeUp (scheduleNextWakeUp t now)
  readMVar mvar
