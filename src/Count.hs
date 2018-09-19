{-# Language TypeFamilies #-}
module Count
  ( Count
  , increment
  , addBit
  ) where

-- base
import Prelude ()
import Control.Monad (mzero)

-- sat-for-games
import Ersatz.Prelude
import Choice

-- | This type represents a natural number and supports operations like
-- increment and comparison.
newtype Count = Count [Bit] deriving Show
-- Invariant: exactly one bit in the list must be true

------------------------------------------------------------------------

instance Codec Count where

  type Decoded Count = Int

  encode i = Count (replicate i false ++ [true])

  decode sol (Count xs) = foldr go (const mzero) xs 0
    where
      go x rec i = do b <- decode sol x
                      if b then return i else rec $! i+1

------------------------------------------------------------------------

instance Choice Count where
  choice (Count f) (Count t) b = Count (go f t)
    where
      go (x:xs) (y:ys) = choice x y b : go xs ys
      go xs     []     = map (not b &&) xs
      go []     ys     = map (b     &&) ys


-- | Increment the value of a 'Count' by one.
increment :: Count -> Count
increment (Count xs) = Count (false : xs)


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
