{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- based on https://github.com/janestreet/base/blob/master/src/or_error.mli
module Data.ErrorOr
  ( ErrorOr (..),
    Taggable (..),
    Failable (..),
    pattern Error,
    pattern OK,
    isOK,
    isError,
    fromOK,
    ok,
    err,
  )
where

import qualified Control.Exception as Exc
import Control.Monad.IO.Class
import Data.Foldable (toList)
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as T

newtype ErrorOr a = ErrorOr {toEither :: Either ErrorAcc a}
  deriving (Show, Read, Eq, Ord, Functor)

pattern OK :: a -> ErrorOr a
pattern OK x <- ErrorOr (Right x)

pattern Error :: ErrorAcc -> ErrorOr a
pattern Error err <- ErrorOr (Left err)

data ErrorAcc
  = List (Seq.Seq ErrorAcc)
  | Tag T.Text ErrorAcc
  | Message T.Text
  deriving (Show, Read, Eq, Ord)

instance Exc.Exception ErrorAcc where
  -- not being used by HUnit/Tasty at least
  displayException = T.unpack . pretty 0

pretty :: Int -> ErrorAcc -> T.Text
pretty indent (Message txt) = T.replicate indent " " <> txt
pretty indent (List errs) = T.intercalate "\n" . map (pretty indent) . toList $ errs
pretty indent (Tag str err) = T.intercalate "\n" [pretty indent (Message str), pretty (indent + 4) err]

instance Semigroup ErrorAcc where
  List l1 <> List l2 = List (l1 <> l2)
  List l1 <> other = List (l1 Seq.|> other)
  other <> List l2 = List (other Seq.<| l2)
  notList1 <> notList2 = List (Seq.fromList [notList1, notList2])

instance Applicative ErrorOr where
  pure x = ok x
  ErrorOr (Right f) <*> ErrorOr (Right a) = ok (f a)
  ErrorOr (Left e1) <*> ErrorOr (Left e2) = ErrorOr . Left $ e1 <> e2
  ErrorOr (Left e1) <*> ErrorOr (Right _) = ErrorOr . Left $ e1
  ErrorOr (Right _) <*> ErrorOr (Left e2) = ErrorOr . Left $ e2

isOK :: ErrorOr a -> Bool
isOK (OK _) = True
isOK _ = False

isError :: ErrorOr a -> Bool
isError = not . isOK

mapError :: (ErrorAcc -> ErrorAcc) -> ErrorOr a -> ErrorOr a
mapError f (ErrorOr (Left e)) = ErrorOr (Left (f e))
mapError _ ok = ok

ok :: a -> ErrorOr a
ok x = ErrorOr (Right x)

fromOK :: ErrorOr a -> a
fromOK (ErrorOr (Right a)) = a
fromOK (ErrorOr (Left err)) = error (T.unpack $ pretty 0 err)

class Taggable c where
  tag :: T.Text -> c -> c

-- I don't think there is a need for this instance actually
instance Taggable ErrorAcc where
  tag = Tag

instance Taggable (ErrorOr a) where
  tag str res
    | isOK res = res
    | otherwise = mapError (Tag str) res

instance Taggable (IO a) where
  tag label = Exc.handle (Exc.throwIO . Tag label)

class ErrorConv t s where
  toE :: t a -> s a

class Failable m where failWith :: ErrorAcc -> m a

instance {-# OVERLAPPING #-} Failable Maybe where failWith _ = Nothing

instance {-# OVERLAPPING #-} Failable ErrorOr where failWith err = ErrorOr (Left err)

instance {-# OVERLAPPING #-} Failable (Either T.Text) where failWith err = Left (pretty 0 err)

instance {-# OVERLAPPABLE #-} MonadIO m => Failable m where failWith err = liftIO (Exc.throwIO err)

err :: Failable m => T.Text -> m a
err = failWith . Message
