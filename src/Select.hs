{-# Language TypeFamilies #-}
{-|
Module      : Select
Description : Symbolic selection of arbitrary values
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Select
  (
  -- * Types
    Select

  -- * Operations
  , runSelect
  , selectList
  , selectPermutationN
  ) where

-- base
import Prelude ()
import Control.Applicative
import Control.Monad
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)

import Ersatz.Prelude
import Choice

-- | A symbolic choice of regular Haskell values.
data Select a
  = Choose (Select a) (Select a) Bit
  | Value a

-- | Delay 'choice' until 'runSelect' or 'decode'.
instance Choice (Select a) where
  choice = Choose

-- | Extract the symbolic value contained in the selection.
runSelect :: Choice a => Select a -> a
runSelect (Value x)      = x
runSelect (Choose f t b) = choice (runSelect f) (runSelect t) b

------------------------------------------------------------------------

-- | Selected value can be transformed with a function.
instance Functor Select where
  fmap = liftA

-- | Multiple selections can be combined into one.
instance Applicative Select where
  pure  = Value
  (<*>) = ap

-- | Selections can be created based on previous selections.
--
-- 'fail' not supported
instance Monad Select where
  Choose f t b >>= k = Choose (f >>= k) (t >>= k) b
  Value x      >>= k = k x

------------------------------------------------------------------------

-- | Create a selection given a list of alternatives.
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
    do b' <- decode solution b
       decode solution (if b' then t else f)

------------------------------------------------------------------------

-- | Lift a comparison on values to be one on symbolic choices of
-- those values. This is used to implement 'Equatable' and 'Orderable'.
liftComparison :: (a -> a -> Bool) -> Select a -> Select a -> Bit
liftComparison (?) x y = runSelect (liftA2 (\a b -> bool (a ? b)) x y)

-- | Lift equality on concrete values to equality on symbolic choices.
instance Eq a => Equatable (Select a) where
  (===) = liftComparison (==)

-- | Lift ordering on concrete values to ordering on symbolic choices.
instance Ord a => Orderable (Select a) where
  (<? ) = liftComparison (< )
  (<=?) = liftComparison (<=)

------------------------------------------------------------------------

-- | Select an ordered permutation from a list of source elements.
selectPermutationN ::
  Int               {- ^ desired permutation length -} ->
  [a]               {- ^ source elements            -} ->
  Ersatz [Select a] {- ^ selected permutation       -}
selectPermutationN n xs =
  case nonEmpty xs of
    Nothing -> return []
    Just xs1 ->
      do ys <- replicateM n (selectList xs1)
         assert $ nor [ sameSelect a b | a:bs <- tails ys, b <- bs ]
         return ys

-- | Helper function for comparing selections for equality for use
-- in 'selectPermutationN'
sameSelect :: Select a -> Select a -> Bit
sameSelect Value{} Value{} = true
sameSelect (Choose f1 t1 b1) (Choose f2 t2 b2) =
  choose (not b1 && sameSelect f1 f2) (b1 && sameSelect t1 t2) b2
sameSelect _ _ = error "sameSelect: mismatched arguments"
