{-# LANGUAGE OverloadedStrings #-}
import Data.ErrorOr
import Data.ErrorOr.Utils (tryRead)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "error-or-utils"
      [ testCase "tryRead reads a value" $
          tryRead " 12 " @?= (pure 12 :: ErrorOr Int),
        testCase "tryRead reports garbage" $
          errorStartsWith "Can't read: abc" (tryRead "abc" :: ErrorOr Int),
        testCase "tryRead reports trailing input, not ambiguity" $
          errorStartsWith "Can't read: 12abc, unconsumed input" (tryRead "12abc" :: ErrorOr Int)
      ]

errorStartsWith :: String -> ErrorOr a -> Assertion
errorStartsWith prefix (Error e) =
  let msg = T.unpack (prettyErrAcc 0 e)
   in assertBool ("unexpected error: " ++ msg) (prefix `isPrefixOf` msg)
errorStartsWith _ _ = assertFailure "expected an error"
