import Control.Concurrent (forkIO)
import Control.Monad
import Data.ErrorOr
import Data.Time
import Data.Validation
import qualified Test.Inbox as Inbox
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
          box <- Inbox.new
          _ <- forkIO (SimTime.threadDelay' tenv 50000 >> Inbox.put box "50ms")
          _ <- forkIO (SimTime.threadDelay' tenv 500000 >> Inbox.put box ".5s")
          _ <- forkIO (SimTime.threadDelay' tenv 1000000 >> Inbox.put box "1s")
          t0 <- Data.Time.getCurrentTime
          Inbox.wait box (Inbox.equalTo "50ms")
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
          return . monadicIO $ do
            t <- run $ SimTime.create (UTCTime (fromGregorian 2015 7 18) 0)
            inbox <- run Inbox.new
            t0 <- run Data.Time.getCurrentTime
            run $ forM_ shuffled (\delay -> void (forkIO (SimTime.threadDelay' t delay >> Inbox.put inbox (show delay))))
            run (Inbox.wait inbox (Inbox.equalTo "50000"))
            elapsed <- run (flip diffUTCTime t0 <$> Data.Time.getCurrentTime)
            run $ toE ((elapsed <! 0.075) <> (elapsed >! 0.049))
            run (SimTime.advance t 0.5)
            run (Inbox.wait inbox (Inbox.equalTo "500000"))
            elapsed <- run (flip diffUTCTime t0 <$> Data.Time.getCurrentTime)
            run $ toE (elapsed <! 0.1)
            run (SimTime.advance t 0.3)
            run (Inbox.wait inbox (Inbox.equalTo "1000000"))
            elapsed <- run (flip diffUTCTime t0 <$> Data.Time.getCurrentTime)
            run $ toE ((elapsed <! 0.4) <> (elapsed >! 0.2))
      ]
