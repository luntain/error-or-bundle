{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
import Control.Concurrent (forkIO, killThread)
import Control.Monad
import qualified Control.Monad.Time as MT
import System.Timeout (timeout)
#if MIN_VERSION_base(4,18,0)
import GHC.Conc (ThreadStatus(..), listThreads, threadStatus)
#endif
import Data.ErrorOr
import Data.Time
import Data.ErrorOr.Validation
import Test.Inbox
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.SimulatedTime as SimTime
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup
#endif

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testCase "MonadTime IO getCurrentTime is the real time" $ do
          t0 <- Data.Time.getCurrentTime
          r <- timeout 1000000 MT.getCurrentTime
          case r of
            Nothing -> assertFailure "timed out"
            Just t -> toE $ (t0 <=! t) <> (diffUTCTime t t0 <! 1)
      , testCase "MonadTime IO threadDelay sleeps" $ do
          r <- timeout 1000000 (MT.threadDelay 1000)
          r @?= Just ()
#if MIN_VERSION_base(4,18,0)
        -- It counts all the threads in the process, so it must not run
        -- concurrently with the other tests (tasty runs tests in parallel
        -- with -N): run it after all of them have finished.
      , after AllFinish "!/does not leak/" $
        testCase "threadDelay' does not leak waker threads" $ do
          tenv <- SimTime.create (UTCTime (fromGregorian 2015 7 18) 0)
          longSleeper <- forkIO (SimTime.threadDelay' tenv (3600 * 1000000))
          SimTime.threadDelay' tenv 1000
          n0 <- liveThreads
          replicateM_ 200 (SimTime.threadDelay' tenv 1000)
          n1 <- liveThreads
          killThread longSleeper
          toE $ tag "thread count" (n1 <=! n0 + 2)
#endif
      , testCase "threadDelay' without time travel" $ do
          tenv <- SimTime.create (UTCTime (fromGregorian 2015 7 18) 0)
          box <- newInbox @String
          _ <- forkIO (SimTime.threadDelay' tenv 50000 >> putInbox box "50ms")
          _ <- forkIO (SimTime.threadDelay' tenv 500000 >> putInbox box ".5s")
          _ <- forkIO (SimTime.threadDelay' tenv 1000000 >> putInbox box "1s")
          t0 <- Data.Time.getCurrentTime
          takeInbox box (equalTo "50ms")
          t1 <- Data.Time.getCurrentTime
          let elapsed = diffUTCTime t1 t0
          toE $ (0.049 <! elapsed) <> (elapsed <! 0.070)

        -- the property runs 100 tests and takes around 5.5s
      , testProperty "advance" $ do
          let sorted :: [Int] =
                [ 50000,
                  10^6,
                  10^7
                ]
          shuffled <- shuffle sorted
          return . monadicIO . run $ do
            fluxCapacitor <- SimTime.create (UTCTime (fromGregorian 2015 7 18) 0)
            inbox <- newInbox
            t0 <- Data.Time.getCurrentTime
            forM_ shuffled $ \delay -> void $ forkIO $ do
                SimTime.threadDelay' fluxCapacitor delay
                putInbox inbox delay
            takeInbox inbox (equalTo 50000)
            elapsed <- flip diffUTCTime t0 <$> Data.Time.getCurrentTime
            toE (elapsed >! 0.049)
            toE =<< expectEmpty inbox
            SimTime.advance fluxCapacitor 1
            takeInbox inbox (equalTo $ 10^6)
            toE . tag "1s" =<< expectEmpty inbox
            SimTime.advance fluxCapacitor 10
            takeInbox inbox (equalTo $ 10^7)
            toE . tag "10s" =<< expectEmpty inbox
      ]

#if MIN_VERSION_base(4,18,0)
liveThreads :: IO Int
liveThreads = length <$> (filterM (fmap alive . threadStatus) =<< listThreads)
  where
    alive ThreadFinished = False
    alive ThreadDied = False
    alive _ = True
#endif
