{-# Language OverloadedStrings #-}
-- | Utilities for data validation
module Data.ErrorOr.Validation
  ( (>!),
    (>=!),
    (<!),
    (<=!),
    (=!),
    (/=!),
    ensure,
    ensureIsNothing,
    ensureIsJust,
    approxEqual,
    ensureAll,
  )
where

import Data.ErrorOr
import qualified Data.Text as T
import Data.Foldable (sequenceA_)

-- <> is infixr 6 :|, which forces parentheses around >! etc, but if I increase
-- priority on >! above 6, it will break relation to the arithmetic operators

infix 4 >!
(>!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a >! b = if a > b then pure () else binaryErr a "is not greater than" b

infix 4 >=!
(>=!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a >=! b = if a >= b then pure () else binaryErr a "is not >= than" b

infix 4 <!
(<!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a <! b = if a < b then pure () else binaryErr a "is not smaller than" b

infix 4 <=!
(<=!) :: (Ord a, Show a) => a -> a -> ErrorOr ()
a <=! b = if a <= b then pure () else binaryErr a "is not <= than" b

infix 4 =!
(=!) :: (Eq a, Show a) => a -> a -> ErrorOr ()
a =! b = if a == b then pure () else binaryErr a "is not equal to" b

infix 4 /=!
(/=!) :: (Eq a, Show a) => a -> a -> ErrorOr ()
a /=! b = if a/=b then pure () else binaryErr a "is equal to" b

-- | Checks the difference of the numbers is less than ratio times
--   the average of the two numbers.
approxEqual :: (RealFrac a, Show a) =>
  -- | ratio
  Double
  -> a
  -> a
  -> ErrorOr ()
approxEqual ratio x y =
  if abs (realToFrac x - realToFrac y) <= abs (ratio * avg)
    then pure ()
    else err ("The numbers are too far apart: " <> T.pack (show x) <> " " <> T.pack (show y))
  where
    avg :: Double
    avg = realToFrac (x + y) / 2

binaryErr :: Show a => a -> T.Text -> a -> ErrorOr ()
binaryErr a label b = fail ((show a) ++ " " ++ T.unpack label ++ " " ++ show b)

ensureIsNothing :: Show a => Maybe a -> ErrorOr ()
ensureIsNothing Nothing = pure ()
ensureIsNothing x = err ("Expected Nothing, but got " <> T.pack (show x))

ensureIsJust :: Maybe a -> ErrorOr ()
ensureIsJust Nothing = err ("Expected Just, but got Nothing")
ensureIsJust (Just _) = pure ()

-- | It annotates a failure with the element's show result.
ensureAll :: Show a => (a -> ErrorOr ()) -> [a] -> ErrorOr ()
ensureAll p = sequenceA_ . map p'
  where p' x = if p x /= pure ()
                  then tag (T.pack $ show x) (p x)
                  else pure ()

ensure :: T.Text -> Bool -> ErrorOr ()
ensure label condition = if condition then pure () else err label
