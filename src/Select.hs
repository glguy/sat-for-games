{-# Language TypeFamilies #-}
module Select where

-- base
import Prelude ()
import Control.Applicative
import Control.Monad
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty((:|)))

import Ersatz.Prelude
import Choice

data Select a
  = Choose Bit (Select a) (Select a)
  | Value a

instance Choice (Select a) where
  choice f t b = Choose b f t

runSelect :: Choice a => Select a -> a
runSelect (Value a) = a
runSelect (Choose b f t) = choice (runSelect f) (runSelect t) b

instance Functor Select where
  fmap = liftA

instance Applicative Select where
  pure  = Value
  (<*>) = ap

instance Monad Select where
  Choose b f t >>= k = Choose b (f >>= k) (t >>= k)
  Value x      >>= k = k x

selectList :: NonEmpty a -> Ersatz (Select a)
selectList (x:|xs) = selectMerge (map Value (x:xs))
  where
    selectMerge [x] = return x
    selectMerge xs = selectMerge =<< selectMergePairs xs

    selectMergePairs (x:y:z) =
      do b <- exists
         z' <- selectMergePairs z
         return (Choose b x y : z')
    selectMergePairs xs = return xs

instance Codec (Select a) where
  type Decoded (Select a) = a
  encode = Value
  decode _ (Value x) = pure x
  decode solution (Choose b f t) =
    do choice <- decode solution b
       decode solution (if choice then t else f)

instance Eq a => Equatable (Select a) where
  x === y = runSelect (liftA2 (\a b -> bool (a == b)) x y)

instance Ord a => Orderable (Select a) where
  x <?  y = runSelect (liftA2 (\a b -> bool (a <  b)) x y)
  x <=? y = runSelect (liftA2 (\a b -> bool (a <= b)) x y)

selectPermutationN :: Int -> [a] -> Ersatz [Select a]
selectPermutationN n xxs =
  case xxs of
    []   -> return []
    x:xs -> do ys <- replicateM n (selectList (x:|xs))
               assert $ nor [ sameSelect a b | a:bs <- tails ys, b <- bs ]
               return ys

sameSelect :: Select a -> Select a -> Bit
sameSelect Value{} Value{} = true
sameSelect (Choose b1 f1 t1) (Choose b2 f2 t2) =
  choose (not b1 && sameSelect f1 f2) (b1 && sameSelect t1 t2) b2
