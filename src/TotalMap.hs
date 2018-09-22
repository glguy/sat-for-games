{-|
Module      : TotalMap
Description : Ordered maps with a default value
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

This module implements a total, ordered map. Upon construction of
a new map, a default value must be specified. This value will be
returned for any lookup that has not otherwise been assigned.

Having a built-in default value is particularly useful when working
with symbolic values as ensures every key has a value, so the structure
of the map will not depend on symbolic values.

To construct new 'TotalMap' values, use 'fromList' and 'pure'.

To update 'TotalMap' values, use ('=:') and 'fmap'.

To query 'TotalMap' values, use ('!').

To combine 'TotalMap' values, use 'choice' and ('<*>').

-}
module TotalMap
  ( TotalMap
  , fromList
  , (!)
  , (=:)
  ) where

-- base
import           Control.Applicative (liftA2)

-- containers
import qualified Data.Map as Map
import           Data.Map (Map)

-- sat-for-games
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
--
-- >>> fromList 0 [('a',1), ('a',2)]
-- fromList 0 [('a',2)]
--
-- >>> fromList 0 [('a',0)]
-- fromList 0 [('a',0)]
fromList ::
  Ord k =>
  v       {- ^ default value            -} ->
  [(k,v)] {- ^ assigned key value pairs -} ->
  TotalMap k v
fromList def xs = TotalMap def (Map.fromList xs)


-- | Lookup the value associated with a given key in the map.
--
-- >>> let m = fromList 0 [('a',1), ('b',2)]
--
-- >>> m ! 'b'
-- 2
--
-- >>> m ! 'c'
-- 0
(!) :: Ord k => TotalMap k v -> k -> v
TotalMap def m ! key = Map.findWithDefault def key m


-- | Assign a new value to a given key in a map.
--
-- Note that this assignment makes no attempt to avoid duplicating
-- the default value.
--
-- >>> let m = fromList 0 [('a',1), ('b',2)]
--
-- >>> ('b' =: 0) m
-- fromList 0 [('a',1),('b',0)]
--
-- >>> ('b' =: 10) m
-- fromList 0 [('a',1),('b',10)]
--
-- >>> ('c' =: 10) m
-- fromList 0 [('a',1),('b',2),('c',10)]
(=:) :: Ord k => k -> v -> TotalMap k v -> TotalMap k v
(k =: v) (TotalMap def m) = TotalMap def (Map.insert k v m)

------------------------------------------------------------------------

-- | Symbolic choice between the values stored at each key.
instance (Ord k, Choice v) => Choice (TotalMap k v) where
  choice x y b = liftA2 (\v u -> choice v u b) x y

------------------------------------------------------------------------

-- | This instance allows you to apply a function to the values
-- stored in the total map.
--
-- >>> fmap (*2) (fromList 10 [('a',20)])
-- fromList 20 [('a',40)]
instance Functor (TotalMap k) where
  fmap f (TotalMap def m) = TotalMap (f def) (fmap f m)

-- | This instance provides point-wise application between
-- two total maps. The meaning of 'pure' is to provide a map
-- that always returns the given value.
--
-- >>> pure 1 :: TotalMap Char Int
-- fromList 1 []
--
-- >>> fromList id [('a',(+1)),('b',subtract 1)] <*> fromList 4 [('b',5), ('c',6)]
-- fromList 4 [('a',5),('b',4),('c',6)]
instance Ord k => Applicative (TotalMap k) where
  pure def = TotalMap def Map.empty
  TotalMap fDef fMap <*> TotalMap xDef xMap =
    TotalMap
      (fDef xDef) -- combine default function and argument
      (Map.mergeWithKey
         (\_key f x -> Just (f x)) -- combine specific function and argument
         (fmap (\f -> f xDef)) -- use default argument
         (fmap (\x -> fDef x)) -- use default function
         fMap xMap)

-- No Monad instance is provided because this would require us to be
-- able to enumerate all of the keys in the first argument to (>>=).
-- Enumerating all of the keys to see what the behavior of the second
-- argument of (>>=) might do with them goes beyond the functionality
-- of the 'TotalMap' type.

------------------------------------------------------------------------

-- | Show 'TotalMap' using 'fromList' syntax.
--
-- >>> show (fromList 0 [('a',1)])
-- "fromList 0 [('a',1)]"
instance (Show k, Show v) => Show (TotalMap k v) where
  showsPrec p (TotalMap def m)
    = showParen (p >= 11)
    $ showString "fromList "
    . showsPrec 11 def
    . showString " "
    . shows (Map.toList m)

-- | Read 'TotalMap' using 'fromList' syntax.
--
-- >>> read "fromList 0 [('a',1)]" :: TotalMap Char Int
-- fromList 0 [('a',1)]
instance (Ord k, Read k, Read v) => Read (TotalMap k v) where
  readsPrec p =
    readParen (p >= 11) $ \s ->
      [ (fromList x y, s3)
        | ("fromList", s1) <- lex s
        , (x,s2) <- readsPrec 11 s1
        , (y,s3) <- readsPrec 11 s2 ]
