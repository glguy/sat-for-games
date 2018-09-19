module TotalMap
  ( TotalMap
  , fromList
  , lookup
  , assign
  , revert
  ) where

import           Prelude hiding (lookup)

import qualified Data.Map as Map
import           Data.Map (Map)

import           Choice (Choice(choice))

-- | A total map behaves like a function that can be efficiently updated.
-- The map tracks a default value when created.
data TotalMap k v = TotalMap v !(Map k v)


-- | Create a 'TotalMap' given a list of key-value pairs
-- and a default value for any unassigned keys. Assignments
-- are processed left-to-right. Later assignments overwrite
-- earlier ones.
--
-- >>> fromList 0 [('a',1), ('b',2)]
-- fromList 0 [('a',1),('b',2)]
-- >>> fromList 0 [('a',1), ('a',2)]
-- fromList 0 [('a',2)]
-- >>> fromList 0 [('a',0)]
-- fromList 0 [('0',0)]
fromList ::
  Ord k =>
  v       {- ^ default value            -} ->
  [(k,v)] {- ^ assigned key value pairs -} ->
  TotalMap k v
fromList def xs = TotalMap def (Map.fromList xs)


-- | Lookup the value associated with a given key in the map.
--
-- >>> let m = fromList 0 [('a',1), ('b',2)]
-- >>> lookup 'b' m
-- 2
-- >>> lookup 'c' m
-- 0
lookup :: Ord k => k -> TotalMap k v -> v
lookup key (TotalMap def m) = Map.findWithDefault def key m


-- | Assign a new value to a given key in a map.
--
-- >>> let m = fromList 0 [('a',1), ('b',2)]
-- >>> lookup 'b' (assign 'b' 10 m)
-- 10
-- >>> lookup 'b' (assign 'c' 10 m)
-- 2
assign :: Ord k => k -> v -> TotalMap k v -> TotalMap k v
assign k v (TotalMap def m) = TotalMap def (Map.insert k v m)


-- | Revert the value at a key back to its default.
--
-- >>> let m = fromList 0 [('a',1), ('b',2)]
-- >>> lookup 'b' (revert 'b' m)
-- 0
-- >>> lookup 'a' (revert 'b' m)
-- 1
revert :: Ord k => k -> TotalMap k v -> TotalMap k v
revert k (TotalMap def m) = TotalMap def (Map.delete k m)


instance (Ord k, Choice v) => Choice (TotalMap k v) where
  choice (TotalMap fDef fMap) (TotalMap tDef tMap) b =
    TotalMap
      (choice fDef tDef b)
      (Map.mergeWithKey
         (\_key f t -> Just (choice f t b))
         (fmap (\f -> choice f tDef b))
         (fmap (\t -> choice fDef t b))
         fMap tMap)


-- | Show 'TotalMap' using 'fromList' syntax.
instance (Show k, Show v) => Show (TotalMap k v) where
  showsPrec p (TotalMap def m)
    = showParen (p >= 11)
    $ showString "fromList "
    . showsPrec 11 def
    . showString " "
    . shows (Map.toList m)
