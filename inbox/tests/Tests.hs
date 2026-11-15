{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception
import Control.Monad (forM_)
import Data.Time (diffUTCTime, getCurrentTime)
import Test.Inbox
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "inbox"
      [ testCase "takeInbox returns a matching message" $ do
          box <- newInbox
          _ <- forkIO (threadDelay 10000 >> putInbox box (1 :: Int) >> putInbox box 2)
          takeInbox box (equalTo 2)
          takeInbox box (equalTo 1),
        testCase "non-matching messages do not extend the timeout" $ do
          box <- newInbox
          -- a non-matching message every 0.1s for 3s
          feeder <- forkIO (forM_ [1 :: Int .. 30] $ \i -> threadDelay 100000 >> putInbox box i)
          t0 <- getCurrentTime
          r <- try (takeInbox' 0.5 box (equalTo 0))
          t1 <- getCurrentTime
          killThread feeder
          case r of
            Left (_ :: ErrorCall) -> pure ()
            Right () -> assertFailure "expected a timeout"
          let elapsed = diffUTCTime t1 t0
          assertBool ("timed out after " ++ show elapsed) (elapsed < 1)
      ]
