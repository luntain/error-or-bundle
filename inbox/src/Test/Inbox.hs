{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Inbox

Facilitates testing of asynchronouse code.

Say you have a server that accepts incoming messages and produces
responses asynchronously. To test it, write some code that puts any
incoming message from the server into an `Inbox` using `putInbox`. The
test can then send messages to the server and make assertions on the
contents of the Inbox according to the expectations on the responses.

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

  , assertEmpty
  , assertEmpty'
)
 where

import qualified Control.Category as Cat
import Control.Arrow (Arrow(..), first)
import Data.IORef (newIORef, readIORef, atomicModifyIORef, IORef)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Data.ErrorOr
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust)
import Control.Monad (unless)
import Data.Foldable (sequenceA_)

-- | An entity holding a number of messages of type `a`
data Inbox a = Inbox (IORef [a])

-- | Create an empty Inbox
newInbox :: IO (Inbox a)
newInbox = Inbox `fmap` newIORef []

-- | Add a message to the Inbox
putInbox :: MonadIO m => Inbox a -> a -> m ()
putInbox (Inbox r) x = liftIO (atomicModifyIORef r ((,()) . (x:)))

-- | `takeInbox'` with a timeout of 3s
takeInbox :: (MonadIO m, Show a) => Inbox a -> Filter a b -> m b
takeInbox = takeInbox' 3

-- | Take a single message out of the inbox, waiting for it up to the specified timeout in seconds.
--   It respects the order the messages were inserted into the inbox.
takeInbox' :: (MonadIO m, Show a) =>
          Float -- ^ timeout in seconds
           -> Inbox a
           -> Filter a b -> m b
takeInbox' sec t@(Inbox r) filter@(Filter text f) = do
  found <- liftIO (atomicModifyIORef r (first reverse . pick f . reverse))
  case found of
    Just x -> return x
    Nothing ->
      if sec < 0.1
        then do
          xs <- liftIO $ readIORef r
          error (T.unpack $ "Timed out waiting for `" <> text <> "`. Contents: " <> (T.pack $ show xs))
        else do
          liftIO (threadDelay (20 * 1000))
          takeInbox' (sec - 0.02) t filter
 where
    pick _ [] = ([], Nothing)
    pick f (x:xs) =
        case f x of
          Nothing ->
              let (rest, res) = pick f xs in
              (x:rest, res)
          Just found -> (xs, Just found)

-- | It is a selector/matcher with a name. The name provides for better error messages.
-- Specifies what message to pick from the `Inbox` and how to transform it. See `predicate`
-- for a `Filter a a` that selects an element and does not apply any transformation.
data Filter a b = Filter T.Text (a -> Maybe b)

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

-- | Assert the Inbox is empty
assertEmpty :: Show a => Inbox a -> IO (ErrorOr ())
assertEmpty (Inbox r) = do
  xs <- readIORef r
  case xs of
    [] -> pure (pure ())
    _ -> return . tag "Unconsumed messages" . sequenceA_ . map (err . T.pack . show) $ xs

-- | Assert that the filter does not match anything in the Inbox
assertEmpty' :: (Show a, MonadIO m) => Inbox a -> Filter a b -> m ()
assertEmpty' (Inbox r) (Filter name p) = do
  elems <- liftIO (filter (isJust.p) <$> readIORef r)
  unless (null elems) $ do
    liftIO
      . toE
      . tag ("There are msgs matching " <> name)
      . sequenceA_
      . map ((err :: T.Text -> ErrorOr ()) . T.pack . show) $ elems
