import Control.Concurrent (forkIO)
import Control.Monad
import Data.ErrorOr
import Data.Time
import Data.Validation
import Test.Inbox
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.SimulatedTime as SimTime
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testCase "put" $ do
          tenv <- SimTime.create (UTCTime (fromGregorian 2015 7 18) 0)
          box <- newInbox
          _ <- forkIO (SimTime.threadDelay' tenv 50000 >> putInbox box "50ms")
          _ <- forkIO (SimTime.threadDelay' tenv 500000 >> putInbox box ".5s")
          _ <- forkIO (SimTime.threadDelay' tenv 1000000 >> putInbox box "1s")
          t0 <- Data.Time.getCurrentTime
          takeInbox box (equalTo "50ms")
          t1 <- Data.Time.getCurrentTime
          let elapsed = diffUTCTime t1 t0
          toE $ (elapsed <! 0.070) <> (elapsed >! 0.049),

        -- This test takes ~23s. The only randomness is the list shuffling,
        -- and it being 3 elements long, there are 3! comibnations. And yet it
        -- randomizes 100times, each taking 0.2s At this point I am not sure
        -- how to restrict the rep count.
        testProperty "advance" $ do
          let sorted =
                [ 50000,
                  500000,
                  1000000
                ]
          shuffled <- shuffle sorted
          return . monadicIO . run $ do
            t <-  SimTime.create (UTCTime (fromGregorian 2015 7 18) 0)
            inbox <- newInbox
            t0 <- Data.Time.getCurrentTime
            forM_ shuffled (\delay -> void (forkIO (SimTime.threadDelay' t delay >> putInbox inbox (show delay))))
            takeInbox inbox (equalTo "50000")
            elapsed <- flip diffUTCTime t0 <$> Data.Time.getCurrentTime
            toE ((elapsed <! 0.085) <> (elapsed >! 0.049))
            SimTime.advance t 0.5
            takeInbox inbox (equalTo "500000")
            elapsed <- flip diffUTCTime t0 <$> Data.Time.getCurrentTime
            toE (elapsed <! 0.1)
            SimTime.advance t 0.3
            takeInbox inbox (equalTo "1000000")
            elapsed <- flip diffUTCTime t0 <$> Data.Time.getCurrentTime
            toE ((elapsed <! 0.4) <> (elapsed >! 0.2))
      ]
