{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Test.SimulatedTime
  ( TimeEnv,
    create,
    advance,
    triggerEvents,
    getSimulatedTime,
    threadDelay',
  )
where

import Control.Concurrent hiding (threadDelay)
import qualified Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Control.Monad.Time
import Data.Foldable (forM_)
import Data.Function (on)
import Data.Functor (void)
import Data.List (insertBy, partition)
import Data.Maybe (listToMaybe)
import Data.Time hiding (getCurrentTime)
import qualified Data.Time
import Data.Bifunctor (Bifunctor(first))

data TimeEnv
  = TimeEnv
      { offset :: TVar NominalDiffTime,
        events :: TVar [(UTCTime, MVar ())]
      }

create :: UTCTime -> IO TimeEnv
create epoch = do
  now <- Data.Time.getCurrentTime
  TimeEnv <$> newTVarIO (diffUTCTime epoch now) <*> newTVarIO []

getSimulatedTime :: TimeEnv -> IO UTCTime
getSimulatedTime t = do
  now <- Data.Time.getCurrentTime
  offset <- readTVarIO (offset t)
  return $ addUTCTime offset now

triggerEvents :: TimeEnv -> IO ()
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
      void (forkIO (Control.Concurrent.threadDelay us >> triggerEvents t))

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
