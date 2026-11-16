{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Test.Inbox

Facilitates testing of asynchronouse code.

== Example
Assume you have a server that accepts incoming messages and
produces responses asynchronously. In order to test it, open a
connection to it, and process any incomming message from the server by
adding it to the Inbox. Now that we know the messages are all arriving
in the inbox, the test can send messages to the server and use
`takeInbox` to wait for expected responses.
-}
module Test.Inbox (

  Inbox
  , newInbox
  , putInbox
  , takeInbox
  , takeInbox'

  , Filter(Filter)
  , equalTo
  , predicate

  , expectEmpty
  , expectEmpty'
)
 where

import qualified Control.Category as Cat
import Control.Arrow (Arrow(..))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (ErrorCall(..), bracket, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ErrorOr
import Data.Foldable (sequenceA_)
import Data.Maybe (isJust)
import qualified Data.Text as T

#if __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail) -- out with the old, Monad.fail
import Control.Monad.Fail (fail) -- in with the new, MonadFail.fail
import Data.Semigroup
#endif


-- | An entity holding a number of messages of type `a`.
newtype Inbox a =
  Inbox (TVar [a]) -- the newest message is at the head

-- | Create an empty Inbox.
newInbox :: IO (Inbox a)
newInbox = Inbox <$> newTVarIO []

-- | Add a message to the Inbox.
putInbox :: MonadIO m => Inbox a -> a -> m ()
putInbox (Inbox r) newmsg = liftIO . atomically $ modifyTVar' r (newmsg :)

-- | 'takeInbox'' with a timeout of 3s
takeInbox :: (MonadIO m, Show a) => Inbox a -> Filter a b -> m b
takeInbox = takeInbox' 3

-- | Take a single message out of the inbox, waiting for it up to the specified timeout in seconds.
--   It respects the order the messages were inserted into the inbox.
takeInbox' ::
  (MonadIO m, Show a) =>
#if __GLASGOW_HASKELL__ >= 808
  -- | timeout in seconds
#endif
  Float ->
  Inbox a ->
  Filter a b ->
  m b
takeInbox' sec (Inbox r) (Filter text f) = liftIO $ do
  res <- withTimer (round $ sec * 10^(6::Int)) $ \timedOut -> atomically $ do
    msgs <- readTVar r
    case pickOldest f msgs of
      Just (remainingMsgs, matched) -> Right matched <$ writeTVar r remainingMsgs
      Nothing -> do
        -- 'retry' blocks this thread until one of the TVars read above
        -- changes, that is until 'putInbox' adds a message or the timer
        -- sets 'timedOut', and then runs the transaction again from the top
        expired <- readTVar timedOut
        if expired then return (Left msgs) else retry
  case res of
    Right matched -> return matched
    Left msgs ->
      throwIO . ErrorCall . T.unpack $
        "Timed out waiting for `" <> text <> "`. Contents: " <> T.pack (show msgs)

-- | Remove the oldest (the last in the list) message accepted by the function.
pickOldest :: (a -> Maybe b) -> [a] -> Maybe ([a], b)
pickOldest _ [] = Nothing
pickOldest f (x:xs) =
  case pickOldest f xs of
    Just (rest, found) -> Just (x:rest, found)
    Nothing -> (xs,) <$> f x

-- | Run the action with a 'TVar' that turns 'True' after the given
-- number of microseconds. It is like 'registerDelay', except it does
-- not require the threaded runtime.
withTimer :: Int -> (TVar Bool -> IO a) -> IO a
withTimer us action = do
  expired <- newTVarIO False
  bracket
    (forkIO $ threadDelay us >> atomically (writeTVar expired True))
    killThread
    (const $ action expired)

-- | It is a selector\/matcher/extractor with a name. It specifies
-- what message to pick from the `Inbox` and how to transform it. The
-- name provides for a better error messages. See `predicate` for a
-- 'Filter' 'a' 'a' that selects an element and does not apply any
-- transformation. Use 'fmap' to transform the result of a filter.
data Filter a b = Filter T.Text (a -> Maybe b)

instance Functor (Filter a) where
  fmap g (Filter name f) = Filter name (fmap g . f)

instance Cat.Category Filter where
  id = Filter "id" Just
  (.) (Filter n1 f1) (Filter n2 f2) = Filter (n2 <> ">>>" <> n1) (\x -> f2 x >>= f1)

instance Arrow Filter where
   arr f = Filter "arr" (Just . f)
   first (Filter name f) = Filter (name <> "*") (\(x,y) -> fmap (,y) (f x))

-- | A filter that matches messages equal to the given one.
equalTo :: (Eq a, Show a) => a -> Filter a ()
equalTo a = Filter (T.pack $ show a) (\x -> if a == x then Just () else Nothing)

-- | A filter that matches messages based on a predicate.
predicate :: T.Text -- ^ name
    -> (a -> Bool) -- ^ the predicate
    -> Filter a a
predicate name p = Filter name (\x -> if p x then Just x else Nothing)

-- | Validate that the inbox has no messages inside at the moment.
expectEmpty :: Show a => Inbox a -> IO (ErrorOr ())
expectEmpty (Inbox r) = do
  xs <- readTVarIO r
  case xs of
    [] -> pure (pure ())
    _ -> return . tag "Unconsumed messages" . sequenceA_ . map (fail . show) $ xs

-- | Validate that the filter does not match anything in the Inbox.
expectEmpty' :: (Show a, MonadIO m) => Inbox a -> Filter a b -> m ()
expectEmpty' (Inbox r) (Filter name p) = do
  elems <- liftIO (filter (isJust.p) <$> readTVarIO r)
  unless (null elems) $ do
    liftIO
      . toE
      . tag ("There are msgs matching " <> name)
      . sequenceA_
      . map (fail . show) $ elems
