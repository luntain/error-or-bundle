{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module ErrorUtils where

import qualified Data.Map as M
import qualified Data.List as List
import qualified Data.Text as T
import Data.ErrorOr


class OverloadedLookup t k v | t -> k, t -> v where overloadedLookup :: k -> t -> Maybe v
instance Eq k => OverloadedLookup [(k,v)] k v where overloadedLookup = List.lookup
instance Ord k => OverloadedLookup (M.Map k v) k v where overloadedLookup = M.lookup

lookup :: (OverloadedLookup t k v, Show k, Show t, Failable m, Applicative m) => k -> t -> m v
lookup k xs = lookupIn (shorten 256 (T.pack $ show xs)) k xs

-- this is a lookup variant for where there is no show instance for the collection
lookupIn :: (OverloadedLookup t k v, Show k, Failable m, Applicative m) => T.Text -> k -> t -> m v
lookupIn name k = maybe (err $ "Can't lookup " <> T.pack (show k) <> " in " <> name) pure . overloadedLookup k

shorten :: Int -> T.Text -> T.Text
shorten maxLen msg =
  case T.splitAt maxLen msg of
    (x, t) | t == T.empty -> x
    (x, _rest) -> T.take (maxLen - 5) x <> "(...)"
