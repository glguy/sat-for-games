{-# Language TypeFamilies #-}
{-|
Module      : Select
Description : Symbolic selection of arbitrary values
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

Symbolic selection from a finite list of alternatives. This module
allows using normal Haskell values symbolically.

We can extract the selected value using either 'runSelect' or
'decode'.

'Select' values can be symbolically compared using comparisons
of their concrete possibilities. For example ('===') will use
('==') on the underlying choices.

>>> :{
solve $
  do let letters = 'a':|"bcdefg"
     x <- selectList letters
     y <- selectList letters
     assert (pure 'c' <? x && x <? y && y <? pure 'f')
     return (x,y)
:}
Just ('d','e')

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

-- sat-for-games
import Ersatz.Prelude
import Choice

-- | A symbolic choice of regular Haskell values.
data Select a
  = Choose (Select a) (Select a) Bit
  | Value a
  deriving Show

-- | Delay 'choice' until 'runSelect' or 'decode'.
instance Choice (Select a) where
  choice = Choose

-- | Extract the symbolic value contained in the selection as long as the
-- underlying values support symbolic choice.
runSelect :: Choice a => Select a -> a
runSelect (Value x)      = x
runSelect (Choose f t b) = choice (runSelect f) (runSelect t) b

------------------------------------------------------------------------

-- | Selected value can be transformed with a function.
instance Functor Select where
  fmap = liftM

-- | Multiple selections can be combined into one.
instance Applicative Select where
  pure  = Value
  (<*>) = ap

-- | Selections can be created based on previous selections.
--
-- 'fail' not supported
instance Monad Select where
  m >>= f = error "Select.Monad not implemented"

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

-- | Lift equality on concrete values to equality on symbolic choices.
instance Eq a => Equatable (Select a) where
  (===) = error "Select.Equatable not implemented"

-- | Lift ordering on concrete values to ordering on symbolic choices.
instance Ord a => Orderable (Select a) where
  (<? ) = error "Select.Orderable not implemented"
  (<=?) = error "Select.Orderable not implemented"

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
