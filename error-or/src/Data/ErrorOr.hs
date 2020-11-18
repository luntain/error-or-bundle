{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- inspired by https://github.com/janestreet/base/blob/master/src/or_error.mli

-- | Provides composable and hierarchical errors, with pretty
-- printing. The errors are accumulated in a tree like structure,
-- 'ErrorAcc'. 'ErrorAcc' is disigned to be read by humans, via
-- 'pretty', not dispatched on by code. Using 'toE' to convert an
-- 'ErrorOr' to IO throws (in case it holds an error) a 'PrettyErrAcc'
-- that uses 'pretty' in the show instance.
module Data.ErrorOr
  ( ErrorOr(..),
    err,
    tag,
    pattern Error,
    pattern OK,
    isOK,
    isError,
    fromOK,
    ErrorConv(..),
    ErrorAcc(..),
    pretty,
    PrettyErrAcc (..),
    tagIO,
  )
where

import qualified Control.Exception as Exc
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.IO.Exception (IOException)

-- | Use 'Applicative'\'s 'sequenceA' and 'sequenceA_' to compose 'ErrorOr's as opposed to 'Monad' derived functions like 'sequence'.
newtype ErrorOr a = ErrorOr {errorOrToEither :: Either ErrorAcc a}
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

pattern OK :: a -> ErrorOr a
pattern OK x <- ErrorOr (Right x)

pattern Error :: ErrorAcc -> ErrorOr a
pattern Error err <- ErrorOr (Left err)

data ErrorAcc
  = Message T.Text
  | List (Seq.Seq ErrorAcc)
  | Tag T.Text ErrorAcc
  deriving (Show, Read, Eq, Ord)

-- | Produce an error
err :: T.Text -> ErrorOr a
err = ErrorOr . Left . Message

-- | Annotate the error with context information.
tag :: T.Text -> ErrorOr a -> ErrorOr a
tag str res
  | isOK res = res
  | otherwise = mapError (Tag str) res

-- | A wrapper over 'ErrorAcc' to provide human readable exceptions.
-- (Exception class' displayException does not seem to be used by GHC)
-- https://stackoverflow.com/questions/55490766/why-doesn-t-ghc-use-my-displayexception-method
newtype PrettyErrAcc = PrettyErrAcc {unPrettyErrAcc :: ErrorAcc}

instance Show PrettyErrAcc where
  show = T.unpack  . pretty 0 . unPrettyErrAcc

instance Exc.Exception PrettyErrAcc where

-- | Tag an exception with an annotation.
--
-- It acts on two types of exceptions: `IOException` and
-- `PrettyErrAcc`. For `PrettyErrAcc` it is streightforward tagging.
-- For `IOException`, otoh, it converts the error message into Text
-- via String and turns it into `PrettyErrAcc` tagged with provided
-- adnotation.
--
-- This is rather a convenience function. Sometimes it is convenient
-- to 'fail "msg"' in IO, and tag it higher up with some context. The
-- need for tagIO often comes up with Data.ErrorOr.Utils.lookup (from
-- error-or-utils package) when used from IO, which is overloaded for
-- MonadFail.
tagIO :: T.Text -> IO a -> IO a
tagIO str action =
  (action
     `Exc.catch` \(e :: IOException) ->
         Exc.throwIO $ PrettyErrAcc $ Tag str (Message (T.pack $ show e)))

     `Exc.catch` \(e :: PrettyErrAcc) ->
         Exc.throwIO $ PrettyErrAcc $ Tag str (unPrettyErrAcc e)


-- | Pretty print the error.
pretty :: Int
  -- ^ Initial indent, usually 0
  -> ErrorAcc -> T.Text
pretty indent (Message txt) = T.replicate indent " " <> txt
pretty indent (List errs) = T.intercalate "\n" . map (pretty indent) . toList $ errs
pretty indent (Tag str err) = T.intercalate "\n" [pretty indent (Message str), pretty (indent + 4) err]

instance Semigroup ErrorAcc where
  List l1 <> List l2 = List (l1 <> l2)
  List l1 <> other = List (l1 Seq.|> other)
  other <> List l2 = List (other Seq.<| l2)
  notList1 <> notList2 = List (Seq.fromList [notList1, notList2])

instance Applicative ErrorOr where
  pure x = ErrorOr (Right x)
  ErrorOr (Right f) <*> ErrorOr (Right a) = pure (f a)
  ErrorOr (Left e1) <*> ErrorOr (Left e2) = ErrorOr . Left $ e1 <> e2
  ErrorOr (Left e1) <*> ErrorOr (Right _) = ErrorOr . Left $ e1
  ErrorOr (Right _) <*> ErrorOr (Left e2) = ErrorOr . Left $ e2

instance Semigroup a => Semigroup (ErrorOr a) where
  Error e1 <> Error e2 = ErrorOr (Left $ e1 <> e2)
  OK v1 <> OK v2 = ErrorOr (Right $ v1 <> v2)
  l@(ErrorOr (Left _)) <> _ = l
  _ <> r = r

instance Monoid a => Monoid (ErrorOr a) where
  mappend = (<>)
  mempty = pure mempty

-- | OrError's instances for 'Monad' and 'Applicative' don't align,
-- but the 'Monad' and 'MonadFail' instances are too useful (as in
-- convenient) to pass on.
-- In particular, composing two failing actions using the
-- 'Applicative' instance creates a /composite/ error, where as
-- composing the same two actions using the 'Monad' instance '(>>)'
-- produces only the error from the first action in the sequence. This
-- is a consequence of the fact that for Monads executing the second
-- of two actions ('(>>)' is defined in terms of '(>>=)') requires the result from the first to be passed to
-- the second: the very result that is not available if the first
-- action fails!
instance Monad ErrorOr where
  return = pure
  ErrorOr either >>= f = ErrorOr (either >>= fmap errorOrToEither f)

instance MonadFail ErrorOr where
  fail = ErrorOr . Left . Message . T.pack

isOK :: ErrorOr a -> Bool
isOK (OK _) = True
isOK _ = False

isError :: ErrorOr a -> Bool
isError = not . isOK

mapError :: (ErrorAcc -> ErrorAcc) -> ErrorOr a -> ErrorOr a
mapError f (ErrorOr (Left e)) = ErrorOr (Left (f e))
mapError _ ok = ok

-- | Like 'fromRight'
fromOK :: ErrorOr a -> a
fromOK (OK a) = a
fromOK (Error err) = error (T.unpack $ pretty 0 err)

-- | Convert between functors that hold error info.
class ErrorConv t s where
  toE :: t a -> s a

-- | Convert from ErrorOr to IO. 'toE' throws a 'PrettyErrAcc' if the input holds an error.
instance ErrorConv ErrorOr IO where
  toE (OK val) = pure val
  toE (Error e) = Exc.throwIO (PrettyErrAcc e)

-- | Convert from 'Maybe a' to 'ErrorOr a'. It converts 'Nothing' simply to an error
-- with msg "Nothing".
instance ErrorConv Maybe ErrorOr where
  toE Nothing = fail "Nothing"
  toE (Just a) = pure a
