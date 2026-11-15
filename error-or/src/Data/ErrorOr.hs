{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
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
-- 'ErrorAcc'. 'ErrorAcc' is designed to be read by humans, via
-- 'prettyErrAcc', not dispatched on by code. Using 'toE' to convert an
-- 'ErrorOr' to IO throws (in case it holds an error) a 'PrettyErrAcc'
-- that uses 'prettyErrAcc' in the show instance.
--
-- This works well for apps where you don't need to display errors in different languages.
module Data.ErrorOr
  ( ErrorOr(..),
    tag,
    pattern Error,
    pattern OK,
    isOK,
    isError,
    fromOK,
    failText,
    ErrorConv(..),
    ErrorAcc(..),
    prettyErrAcc,
    PrettyErrAcc (..),
    tagIO,
  )
where

import qualified Control.Exception as Exc
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
#if __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
import Data.Semigroup
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.Except (MonadError, throwError, catchError) -- it exports GHC.Base.fail (?!)
import Data.String
import System.Exit (ExitCode)

-- | Compose 'ErrorOr's with 'Applicative' functions ('Data.Foldable.traverse_',
-- 'Data.Foldable.for_', 'Data.Foldable.sequenceA_', '<*>', '*>') to collect all
-- the errors. The 'Monad'-based functions ('mapM_', 'Data.Foldable.forM_',
-- 'sequence_', '>>=', '>>') stop at the first
-- error. See the note on the 'Monad' instance.
newtype ErrorOr a = ErrorOr {errorOrToEither :: Either ErrorAcc a}
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

pattern OK :: a -> ErrorOr a
pattern OK x <- ErrorOr (Right x)

pattern Error :: ErrorAcc -> ErrorOr a
pattern Error err = ErrorOr (Left err)

{-# COMPLETE OK, Error #-}

data ErrorAcc
  = ErrMessage T.Text
  | ErrList (Seq.Seq ErrorAcc)
  | ErrTag T.Text ErrorAcc
  deriving (Show, Read, Eq, Ord)

instance IsString ErrorAcc where
  fromString = ErrMessage . T.pack

-- | Produce an error from `Text`. You can also use `fail`, which takes a `String`.
failText :: T.Text -> ErrorOr a
failText = ErrorOr . Left . ErrMessage

-- | Annotate the error with context information.
tag :: T.Text -> ErrorOr a -> ErrorOr a
tag str res
  | isOK res = res
  | otherwise = mapError (ErrTag str) res

-- | A wrapper over 'ErrorAcc' to provide human readable exceptions.
-- (Exception class' displayException does not seem to be used by GHC)
-- https://stackoverflow.com/questions/55490766/why-doesn-t-ghc-use-my-displayexception-method
newtype PrettyErrAcc = PrettyErrAcc {unPrettyErrAcc :: ErrorAcc}

instance Show PrettyErrAcc where
  show = T.unpack  . prettyErrAcc 0 . unPrettyErrAcc

instance Exc.Exception PrettyErrAcc where

-- | Tag an exception with an annotation.
--
-- It lets async exceptions and 'ExitCode' (thrown by 'System.Exit.exitWith'
-- and friends) pass unchanged, and has special logic for
-- `PrettyErrAcc` for better composition. For other exceptions, it
-- converts the error message into Text via String and turns it into
-- `PrettyErrAcc` tagged with the provided annotation. For `PrettyErrAcc`
-- it is straightforward tagging.
--
-- __Use it at boundaries__, where an exception is about to be reported
-- (around @main@, a request handler, a job), not deep inside code whose
-- exceptions someone handles by type. Wrapping erases the type of the
-- original exception, so handlers further up that match a specific type
-- no longer fire. For example, after
--
-- @
-- tagIO \"loading config\" (readFile path) \`catch\` \\(e :: IOException) -> ...
-- @
--
-- the handler never runs, because what escapes is a 'PrettyErrAcc'. The
-- same goes for retry logic keyed on a library's exception type, and for
-- frameworks that use exceptions for control flow (redirects, early
-- responses).
--
-- Whether an exception is asynchronous is decided by its type (whether it
-- is wrapped in 'Exc.SomeAsyncException'), not by how it was delivered:
--
--   * 'Exc.ThreadKilled' (from 'Control.Concurrent.killThread'), the
--     timeout of "System.Timeout", and the @async@ package's
--     @AsyncCancelled@ and @ExceptionInLinkedThread@ are asynchronous and
--     pass through untagged. In particular, a failure of a child that
--     reaches this thread via the @async@ package's @link@ is not tagged,
--     while the same failure observed via @wait@ is.
--
--   * An exception of an ordinary type sent from another thread with
--     'Control.Concurrent.throwTo' is treated as synchronous and gets
--     wrapped, so a thread that interrupts this one that way cannot catch
--     its own exception type afterwards.
--
--   * Exceptions thrown by the runtime on deadlock
--     ('Exc.BlockedIndefinitelyOnMVar', 'Exc.BlockedIndefinitelyOnSTM',
--     'Exc.Deadlock') or on @\<\<loop\>\>@ ('Exc.NonTermination') are
--     typed as synchronous and get wrapped.
--
-- Only exceptions raised in the calling thread while the action runs are
-- seen. Failures of threads forked inside the action are not, unless
-- something brings them back (@wait@, @link@, @concurrently@, ...).
--
-- __Alternative on GHC 9.10 and later:__ 'Control.Exception.annotateIO'
-- attaches an annotation to the exception's context without changing its
-- type, so handlers that match by type keep working. The annotations are
-- printed for uncaught exceptions and by 'Exc.displayException' on
-- 'Exc.SomeException' (as checked on GHC 9.10.3), but not by 'show'. They
-- are a flat list rather than a tree like 'ErrorAcc', and they are lost
-- when an exception is caught at its specific type and rethrown with
-- 'Exc.throwIO'. That makes them the better choice for annotating deep
-- inside code; 'tagIO' remains useful at reporting boundaries and on
-- older compilers.
--
-- @since 0.1.1.0
tagIO :: T.Text -> IO a -> IO a
tagIO ctx action =
  action
     `Exc.catch` \(e :: Exc.SomeException) ->
         case Exc.fromException e of
           Just (pretty :: PrettyErrAcc) ->
             Exc.throwIO $ PrettyErrAcc $ ErrTag ctx (unPrettyErrAcc pretty)
           Nothing ->
            if isAsyncException e || isExitCode e
              then Exc.throwIO e
              else Exc.throwIO $ PrettyErrAcc $ ErrTag ctx (ErrMessage (T.pack $ show e))
  where
    isAsyncException :: Exc.SomeException -> Bool
    isAsyncException e =
      case Exc.fromException e of
          Just (Exc.SomeAsyncException _) -> True
          Nothing -> False
    isExitCode :: Exc.SomeException -> Bool
    isExitCode e =
      case Exc.fromException e of
          Just (_ :: ExitCode) -> True
          Nothing -> False

-- | Pretty print the error.
prettyErrAcc :: Int
  -- ^ Initial indent, usually 0
  -> ErrorAcc -> T.Text
prettyErrAcc indent (ErrMessage txt) = T.intercalate "\n" . map (T.replicate indent " " <>) . T.splitOn "\n" $ txt
prettyErrAcc indent (ErrList errs) = T.intercalate "\n" . map (prettyErrAcc indent) . toList $ errs
prettyErrAcc indent (ErrTag str err) = T.intercalate "\n" [prettyErrAcc indent (ErrMessage str), prettyErrAcc (indent + 4) err]

instance Semigroup ErrorAcc where
  ErrList l1 <> ErrList l2 = ErrList (l1 <> l2)
  ErrList l1 <> other = ErrList (l1 Seq.|> other)
  other <> ErrList l2 = ErrList (other Seq.<| l2)
  notList1 <> notList2 = ErrList (Seq.fromList [notList1, notList2])

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

instance (
#if __GLASGOW_HASKELL__ < 808
    Semigroup (ErrorOr a),
#endif
    Monoid a) => Monoid (ErrorOr a) where
  mappend = (<>)
  mempty = pure mempty

-- | ErrorOr's instances for 'Monad' and 'Applicative' don't align,
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
  fail = ErrorOr . Left . ErrMessage . T.pack

isOK :: ErrorOr a -> Bool
isOK (OK _) = True
isOK _ = False

isError :: ErrorOr a -> Bool
isError = not . isOK

mapError :: (ErrorAcc -> ErrorAcc) -> ErrorOr a -> ErrorOr a
mapError f (ErrorOr (Left e)) = ErrorOr (Left (f e))
mapError _ ok = ok

-- | A partial function, like 'fromRight'.
fromOK :: ErrorOr a -> a
fromOK (OK a) = a
fromOK (Error err) = error (T.unpack $ prettyErrAcc 0 err)

instance MonadError ErrorAcc ErrorOr where
  -- | Usually it is more convenient to use `fail` than this method.
  throwError err = ErrorOr (Left err)
  catchError action handler =
    case action of
      OK _ -> action
      Error err -> handler err

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

-- | Convert from `Either e`, `Left` becomes an error.
instance Show e => ErrorConv (Either e) ErrorOr where
  toE (Left err) = fail (show err)
  toE (Right a) = pure a
