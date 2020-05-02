module Inbox where

import MyPrelude hiding (first)
import qualified Control.Category as Cat
import qualified Err
import Control.Arrow (first)

data T a = T (IORef [a])

new :: IO (T a)
new = T `fmap` newIORef []

put :: MonadIO m => T a -> a -> m ()
put (T r) x = liftIO (atomicModifyIORef r ((,()) . (x:)))

-- default timeout 3s
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
          error ("Timed out waiting for `" ++ text ++ "`. Contents: " ++ show xs)
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
      . throwIO
      . Err.tag ("There are msgs matching " ++ name)
      . Err.List
      . map (Err.Msg . show) $ elems


data Filter a b = Filter String (a -> Maybe b)

instance Cat.Category Filter where
  id = Filter "id" Just
  (.) (Filter n1 f1) (Filter n2 f2) = Filter (n2 ++ ">>>" ++ n1) (\x -> f2 x >>= f1)

instance Arrow Filter where
   arr f = Filter "arr" (Just . f)
   first (Filter name f) = Filter (name ++ "*") (\(x,y) -> fmap (,y) (f x))

allClear :: Show a => T a -> IO (Result ())
allClear (T r) = do
  xs <- readIORef r
  case xs of
    [] -> return (Right ())
    _ -> return . tagResult "Unconsumed messages" . mconcat . map (errorResult . show) $ xs


equalTo :: (Eq a, Show a) => a -> Filter a ()
equalTo a = Filter (show a) (\x -> if a == x then Just () else Nothing)

predicate :: String -> (a -> Bool) -> Filter a a
predicate name p = Filter name (\x -> if p x then Just x else Nothing)
