{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
import Control.Concurrent (forkIO)
import Control.Monad
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
      [ testCase "put" $ do
          tenv <- SimTime.create (UTCTime (fromGregorian 2015 7 18) 0)
          box <- newInbox @String
          _ <- forkIO (SimTime.threadDelay' tenv 50000 >> putInbox box "50ms")
          _ <- forkIO (SimTime.threadDelay' tenv 500000 >> putInbox box ".5s")
          _ <- forkIO (SimTime.threadDelay' tenv 1000000 >> putInbox box "1s")
          t0 <- Data.Time.getCurrentTime
          takeInbox box (equalTo "50ms")
          t1 <- Data.Time.getCurrentTime
          let elapsed = diffUTCTime t1 t0
          toE $ (elapsed <! 0.070) <> (elapsed >! 0.049),

        -- takes around 5.5s
        testProperty "advance" $ do
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
