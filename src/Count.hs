{-# Language TypeFamilies #-}
{-|
Module      : Count
Description : Symbolic operations for counting increments and decrements.
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

This type allows us to symbolically count numbers incrementing and decrementing
by one. Counts can be ordered and compared for equality.

Use 'encode' to make constant valued counts.

Use 'increment', 'decrement', and 'addBit' to update 'Count' values.

>>> :{
solve $
  do (x,y,z) <- exists
     let count = foldr addBit (encode 0)
     assert (count [x,y,z] === encode 2)
     assert (count [x,  z] === encode 1)
     assert (count [  y,z] === encode 1)
     return (x,y,z)
:}
Just (True,True,False)

-}
module Count
  ( Count
  , increment
  , decrement
  , addBit
  ) where

-- base
import Prelude ()
import Control.Monad (mzero)
import Data.List (elemIndex)

-- sat-for-games
import Ersatz.Prelude
import Choice

-- | This type represents a natural number and supports operations like
-- increment and comparison.
newtype Count = Count [Bit] deriving Show
-- Invariant: exactly one bit in the list must be true

------------------------------------------------------------------------

-- | 'encode' treats negative inputs as zero
instance Codec Count where

  type Decoded Count = Int

  encode i = error "Codec.Count not implemented"

  decode sol (Count xs) = error "Codec.Count not implemented"

------------------------------------------------------------------------

instance Choice Count where
  choice (Count f) (Count t) b = Count (go f t)
    where
      go (x:xs) (y:ys) = choice x y b : go xs ys
      go xs     []     = map (not b &&) xs
      go []     ys     = map (    b &&) ys

------------------------------------------------------------------------

-- | Increment the value of a 'Count' by one.
increment :: Count -> Count
increment (Count xs) = error "increment not implemented"

-- | Decrement the value of a 'Count' by one. Decrement of zero is zero.
decrement :: Count -> Count
decrement (Count xs) =
  case xs of
    b0:b1:bs -> Count ((b0 || b1) : bs)
    _        -> Count [true]

-- | Add a bit representing a zero or one to the count.
addBit :: Bit -> Count -> Count
addBit bit count = error "addBit not implemented"

------------------------------------------------------------------------

instance Equatable Count where
  Count x === Count y = and (zipWith (===) x y)

------------------------------------------------------------------------

-- | Construct a comparison on 'Count' given a function with access to
-- three pieces of information:
--
-- 1. Is the left argument zero?
--
-- 2. Is the right argument zero?
--
-- 3. When neither argument is zero, what is the result of comparing
--    each number decremented by one.
foldComparison ::
  Bit                        {- ^ base case                                   -} ->
  (Bit -> Bit -> Bit -> Bit) {- ^ left is zero, right is zero, recursive case -} ->
  Count -> Count -> Bit      {- ^ comparison function                         -}
foldComparison z f (Count l) (Count r) = foldr (uncurry f) z (zip l r)

instance Orderable Count where
  (<? ) = foldComparison false $ \x y next -> not y && (x || next)
  (<=?) = foldComparison true  $ \x y next -> x || not y && next
