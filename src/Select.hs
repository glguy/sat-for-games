{-# Language TypeFamilies #-}
{-|
Module      : Select
Description : Symbolic selection of arbitrary values
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Select where

-- base
import Prelude ()
import Control.Applicative
import Control.Monad
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))

import Ersatz.Prelude
import Choice

data Select a
  = Choose (Select a) (Select a) Bit
  | Value a

instance Choice (Select a) where
  choice = Choose

runSelect :: Choice a => Select a -> a
runSelect (Value x)      = x
runSelect (Choose f t b) = choice (runSelect f) (runSelect t) b

------------------------------------------------------------------------

instance Functor Select where
  fmap = liftA

instance Applicative Select where
  pure  = Value
  (<*>) = ap

instance Monad Select where
  Choose f t b >>= k = Choose (f >>= k) (t >>= k) b
  Value x      >>= k = k x

------------------------------------------------------------------------

selectList :: NonEmpty a -> Ersatz (Select a)
selectList xxs = selectMerge (fmap Value xxs)
  where
    selectMerge :: NonEmpty (Select a) -> Ersatz (Select a)
    selectMerge (x:|[])   = return x
    selectMerge (x:|y:ys) =
      do b    <- exists
         rest <- selectMergePairs ys
         selectMerge (Choose x y b :| rest)

    selectMergePairs :: [Select a] -> Ersatz [Select a]
    selectMergePairs (x:y:z) =
      do b  <- exists
         z' <- selectMergePairs z
         return (Choose x y b : z')
    selectMergePairs xs = return xs


instance Codec (Select a) where
  type Decoded (Select a) = a
  encode = Value
  decode _ (Value x) = pure x
  decode solution (Choose f t b) =
    do choice <- decode solution b
       decode solution (if choice then t else f)

------------------------------------------------------------------------

liftComparison :: (a -> a -> Bool) -> Select a -> Select a -> Bit
liftComparison (?) x y = runSelect (liftA2 (\a b -> bool (a ? b)) x y)

instance Eq a => Equatable (Select a) where
  (===) = liftComparison (==)

instance Ord a => Orderable (Select a) where
  (<? ) = liftComparison (< )
  (<=?) = liftComparison (<=)

------------------------------------------------------------------------

selectPermutationN :: Int -> [a] -> Ersatz [Select a]
selectPermutationN n xxs =
  case xxs of
    []   -> return []
    x:xs -> do ys <- replicateM n (selectList (x:|xs))
               assert $ nor [ sameSelect a b | a:bs <- tails ys, b <- bs ]
               return ys

sameSelect :: Select a -> Select a -> Bit
sameSelect Value{} Value{} = true
sameSelect (Choose f1 t1 b1) (Choose f2 t2 b2) =
  choose (not b1 && sameSelect f1 f2) (b1 && sameSelect t1 t2) b2
