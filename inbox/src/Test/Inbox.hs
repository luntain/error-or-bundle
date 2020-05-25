{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Inbox where

import qualified Control.Category as Cat
import Control.Arrow (Arrow(..), first)
import Data.IORef (newIORef, readIORef, atomicModifyIORef, IORef)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Data.ErrorOr
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust)
import Control.Monad (unless)
import Control.Exception (throwIO)
import Data.Foldable (sequenceA_)

data T a = T (IORef [a])

new :: IO (T a)
new = T `fmap` newIORef []

put :: MonadIO m => T a -> a -> m ()
put (T r) x = liftIO (atomicModifyIORef r ((,()) . (x:)))

-- | Wait with a timeout of 3s
wait :: (MonadIO m, Show a) => T a -> Filter a b -> m b
wait = wait' 3

wait' :: (MonadIO m, Show a) => Float -> T a -> Filter a b -> m b
wait' sec t@(T r) filter@(Filter text f) = do
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
          wait' (sec - 0.02) t filter
 where
    pick _ [] = ([], Nothing)
    pick f (x:xs) =
        case f x of
          Nothing ->
              let (rest, res) = pick f xs in
              (x:rest, res)
          Just found -> (xs, Just found)

zero :: (Show a, MonadIO m) => T a -> Filter a b -> m ()
zero (T r) (Filter name p) = do
  elems <- liftIO (filter (isJust.p) <$> readIORef r)
  unless (null elems) $ do
    liftIO
      . toE
      . tag ("There are msgs matching " <> name)
      . sequenceA_
      . map ((err :: T.Text -> ErrorOr ()) . T.pack . show) $ elems


-- | It is a selector/matcher with a name. Name is here for better error messages
data Filter a b = Filter T.Text (a -> Maybe b)

instance Cat.Category Filter where
  id = Filter "id" Just
  (.) (Filter n1 f1) (Filter n2 f2) = Filter (n2 <> ">>>" <> n1) (\x -> f2 x >>= f1)

instance Arrow Filter where
   arr f = Filter "arr" (Just . f)
   first (Filter name f) = Filter (name <> "*") (\(x,y) -> fmap (,y) (f x))

allClear :: Show a => T a -> IO (ErrorOr ())
allClear (T r) = do
  xs <- readIORef r
  case xs of
    [] -> pure (pure ())
    _ -> return . tag "Unconsumed messages" . sequenceA_ . map (err . T.pack . show) $ xs


equalTo :: (Eq a, Show a) => a -> Filter a ()
equalTo a = Filter (T.pack $ show a) (\x -> if a == x then Just () else Nothing)

predicate :: T.Text -> (a -> Bool) -> Filter a a
predicate name p = Filter name (\x -> if p x then Just x else Nothing)
