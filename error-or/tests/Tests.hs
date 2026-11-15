{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception
import Data.ErrorOr
import Data.Foldable (traverse_)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import System.Exit
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "error-or"
      [ testCase "Applicative accumulates errors" $
          (failText "a" *> failText "b" :: ErrorOr ())
            @?= Error (ErrList (Seq.fromList [ErrMessage "a", ErrMessage "b"])),
        testCase "traverse_ accumulates errors" $
          isErrorWith "a\nb" (traverse_ failText ["a", "b"] :: ErrorOr ()),
        testCase "prettyErrAcc indents every line of a message" $
          prettyErrAcc 0 (ErrTag "ctx" (ErrMessage "one\ntwo"))
            @?= "ctx\n    one\n    two",
        testCase "tagIO tags synchronous exceptions" $ do
          r <- try (tagIO "outer" (tagIO "inner" (throwIO (userError "boom"))))
          case r of
            Left (PrettyErrAcc e) ->
              prettyErrAcc 0 e @?= "outer\n    inner\n        user error (boom)"
            Right () -> assertFailure "expected an exception",
        testCase "tagIO lets ExitCode through" $ do
          r :: Either ExitCode () <- try (tagIO "ctx" (exitWith (ExitFailure 3)))
          r @?= Left (ExitFailure 3),
        testCase "tagIO lets async exceptions through" $ do
          started <- newEmptyMVar
          result <- newEmptyMVar
          tid <- forkIO $ do
            r <- try (tagIO "ctx" (putMVar started () >> threadDelay 10000000))
            putMVar result r
          takeMVar started
          killThread tid
          r <- takeMVar result
          case r of
            Left (e :: SomeException) ->
              fromException e @?= Just ThreadKilled
            Right () -> assertFailure "expected an exception"
      ]

isErrorWith :: T.Text -> ErrorOr a -> Assertion
isErrorWith expected (Error e) = prettyErrAcc 0 e @?= expected
isErrorWith _ _ = assertFailure "expected an error"
