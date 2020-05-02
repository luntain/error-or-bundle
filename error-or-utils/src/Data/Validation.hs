{-# Language OverloadedStrings #-}
module Data.Validation where

import Data.ErrorOr
import qualified Data.Text as T
import Data.Foldable (sequenceA_)

-- <> is infixr 6 :|

infix 4 >!
(>!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a >! b = if a > b then ok () else binaryErr a "is not greater than" b

infix 4 >=!
(>=!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a >=! b = if a >= b then ok () else binaryErr a "is not >= than" b

infix 4 <!
(<!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a <! b = if a < b then ok () else binaryErr a "is not smaller than" b

infix 4 <=!
(<=!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a <=! b = if a <= b then ok () else binaryErr a "is not <= than" b

infix 4 =!
(=!) :: (Eq a, Show a) => a -> a -> ErrorOr ()
a =! b = if a == b then ok () else binaryErr a "is not equal to" b

infix 4 /=!
(/=!) :: (Eq a, Show a) => a -> a -> ErrorOr ()
a /=! b = if a/=b then ok () else binaryErr a "is equal to" b

binaryErr :: Show a => a -> T.Text -> a ->  ErrorOr ()
binaryErr a label b = err (T.pack (show a) <> " " <> label <> " " <> T.pack (show b))

ensureIsNothing :: Show a => Maybe a -> ErrorOr ()
ensureIsNothing Nothing = ok ()
ensureIsNothing x = err ("Expected Nothing, but got " <> T.pack (show x))

ensureIsJust :: Maybe a -> ErrorOr ()
ensureIsJust Nothing = err ("Expected Just, but got Nothing")
ensureIsJust (Just _) = ok ()

ensureAll :: Show a => (a -> ErrorOr ()) -> [a] -> ErrorOr ()
ensureAll p = sequenceA_ . map p'
  where p' x = if p x /= ok ()
                  then tag (T.pack $ show x) (p x)
                  else ok ()

ensure :: T.Text -> Bool -> ErrorOr ()
ensure label condition = if condition then ok () else err label

approxEqual :: (RealFrac a, Show a) => Double -> a -> a -> ErrorOr ()
approxEqual ratio x y =
  if abs (realToFrac x - realToFrac y) <= abs (ratio * avg)
    then ok ()
    else err ("The numbers are too far apart: " <> T.pack (show x) <> " " <> T.pack (show y))
  where
    avg :: Double
    avg = realToFrac (x + y) / 2
