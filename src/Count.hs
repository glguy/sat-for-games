{-# Language TypeFamilies #-}
{-|
Module      : Count
Description : Symbolic operations for counting increments and decrements.
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com
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

  encode i = Count (replicate i false ++ [true])

  decode sol (Count xs) =
    do xs' <- traverse (decode sol) xs
       case elemIndex True xs' of
         Nothing -> mzero
         Just i  -> return i

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
increment (Count xs) = Count (false : xs)

-- | Decrement the value of a 'Count' by one. Decrement of zero is zero.
decrement :: Count -> Count
decrement (Count xs) =
  case xs of
    b0:b1:bs -> Count ((b0 || b1) : bs)
    _        -> Count [true]

-- | Add a bit representing a zero or one to the count.
addBit :: Count -> Bit -> Count
addBit count = choice count (increment count)

------------------------------------------------------------------------

instance Equatable Count where
  Count x === Count y = and (zipWith (===) x y)

------------------------------------------------------------------------

instance Orderable Count where
  Count l <? Count r = go l r
    where
      go (x:xs) (y:ys) = not y && (x || go xs ys)
      go _ _           = false
