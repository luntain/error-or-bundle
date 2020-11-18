{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Contains a simulated-time implementaion. It can be used directly,
-- to implement the simulated versions of getCurrentTime and
-- threadDelay, or can be used via monad transformers defined in
-- `Control.Monad.Trans.SimulatedTime`
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
import Data.Foldable (forM_)
import Data.Function (on)
import Data.Functor (void)
import Data.List (insertBy, partition)
import Data.Maybe (listToMaybe)
import Data.Time hiding (getCurrentTime)
import qualified Data.Time
import Data.Bifunctor (Bifunctor(first))

-- | It remembers the offset from system time and keeps a list of
-- sleeping threads (threads in threadDelay call)
data TimeEnv
  = TimeEnv
      { offset :: TVar NominalDiffTime,
        events :: TVar [(UTCTime, MVar ())]
      }

-- | Create the simulated time env from the given time start point. For example
-- timeEnv <- create (fromGregorian 2000 1 1) 0
-- getSimulatedTime timeEnv
create :: UTCTime -> IO TimeEnv
create epoch = do
  now <- Data.Time.getCurrentTime
  TimeEnv <$> newTVarIO (diffUTCTime epoch now) <*> newTVarIO []

-- | The current simulated time
getSimulatedTime :: TimeEnv -> IO UTCTime
getSimulatedTime t = do
  now <- Data.Time.getCurrentTime
  offset <- readTVarIO (offset t)
  return $ addUTCTime offset now

-- | Wake up due sleeping threads, based on TimeEnv. This happens on
-- its own, but this call can quicken things up. Under the hood it is
-- just `advance tenv 0`
triggerEvents :: TimeEnv -> IO ()
triggerEvents t = advance t 0

-- | Move the simulated time by a delta
advance :: TimeEnv -> NominalDiffTime -> IO ()
advance t diff = do
  now <- Data.Time.getCurrentTime
  due <- atomically $ do
    unless (diff == 0) $ modifyTVar (offset t) (+ diff)
    modifyTVar (events t) (map (first (addUTCTime (- diff))))
    extractDueEvents t now
  forM_ due (flip putMVar ())
  scheduleNextWakeUp t now

-- the 'now' is the system time now, as opposed to simulated now
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

-- | Simulated alternative to `Control.Concurrent.threadDelay`, sleep
-- for the given number of microseconds. Threads wakes up according to
-- the simulated time.
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
