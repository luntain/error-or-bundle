{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NumericUnderscores #-}

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
import Data.List (insertBy, partition)
import Data.Maybe (listToMaybe)
import Data.Time hiding (getCurrentTime)
import qualified Data.Time
import Data.Bifunctor (Bifunctor(first))

-- | It remembers the offset from system time and keeps a list of
-- sleeping threads (threads in 'threadDelay'' call)
data TimeEnv
  = TimeEnv
      { offset :: TVar NominalDiffTime,
        -- | sorted by wake-up time, which is in system time
        events :: TVar [(UTCTime, MVar ())],
        -- | The thread that will wake up the earliest event and when (in
        -- system time). There is at most one such thread at a time.
        waker :: MVar (Maybe (UTCTime, ThreadId))
      }

-- | Create the simulated time env from the given time start point. For example
--
-- @
-- timeEnv <- create (UTCTime (fromGregorian 2000 1 1) 0)
-- getSimulatedTime timeEnv
-- @
create :: UTCTime -> IO TimeEnv
create epoch = do
  now <- Data.Time.getCurrentTime
  TimeEnv <$> newTVarIO (diffUTCTime epoch now) <*> newTVarIO [] <*> newMVar Nothing

-- | The current simulated time
getSimulatedTime :: TimeEnv -> IO UTCTime
getSimulatedTime t = do
  now <- Data.Time.getCurrentTime
  offset <- readTVarIO (offset t)
  return $ addUTCTime offset now

-- | Wake up due sleeping threads, based on TimeEnv. This happens on
-- its own, but this call can quicken things up. Under the hood it is
-- just @advance tenv 0@
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
  scheduleNextWakeUp t

-- the 'now' is the system time now, as opposed to simulated now
extractDueEvents :: TimeEnv -> UTCTime -> STM [MVar ()]
extractDueEvents t now = do
  (due, inTheFuture) <- partition ((<= now) . fst) <$> readTVar (events t)
  writeTVar (events t) inTheFuture
  return (map snd due)

-- | Make sure the registered waker thread fires no later than the
-- earliest event. A waker scheduled for a later time is killed and
-- replaced; one that is no longer needed is killed.
--
-- A waker unregisters itself (under the 'waker' lock) before it does
-- any work, so a registered waker is always still sleeping and is safe
-- to kill.
scheduleNextWakeUp :: TimeEnv -> IO ()
scheduleNextWakeUp t =
  modifyMVar_ (waker t) $ \current -> do
    nextEvent <- listToMaybe . map fst <$> readTVarIO (events t)
    case (nextEvent, current) of
      (Just tm, Just (scheduled, _)) | scheduled <= tm -> return current
      (Just tm, _) -> do
        forM_ current (killThread . snd)
        tid <- forkIO (wakeUpAt tm)
        return (Just (tm, tid))
      (Nothing, _) -> do
        forM_ current (killThread . snd)
        return Nothing
  where
    wakeUpAt tm = do
      now <- Data.Time.getCurrentTime
      Control.Concurrent.threadDelay (ceiling (diffUTCTime tm now * 1_000_000))
      me <- myThreadId
      stillRegistered <- modifyMVar (waker t) $ \current ->
        return $
          if fmap snd current == Just me
            then (Nothing, True)
            else (current, False)
      when stillRegistered (triggerEvents t)

-- | Simulated alternative to `Control.Concurrent.threadDelay`, sleep
-- for the given number of microseconds. Threads wakes up according to
-- the simulated time.
threadDelay' :: TimeEnv -> Int -> IO ()
threadDelay' t us = do
  mvar <- newEmptyMVar
  now <- Data.Time.getCurrentTime
  let eventTime = addUTCTime (fromIntegral us / (1000 * 1000)) now
  atomically $ modifyTVar (events t) (insertBy (compare `on` fst) (eventTime, mvar))
  scheduleNextWakeUp t
  readMVar mvar
