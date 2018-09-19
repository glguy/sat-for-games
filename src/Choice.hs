module Choice where

-- base
import Data.Bool (bool)

-- ersatz
import Ersatz (Bit, choose)

-- | Class for types that allow a value to be symbolically selected
-- from a pair of alternatives based on the value of a 'Bit'.
--
-- This is a generalization of the 'choose' function on 'Bit'
class Choice a where
  choice ::
    a   {- ^ false branch -} ->
    a   {- ^ true  branch -} ->
    Bit {- ^ selector     -} ->
    a

instance Choice Bit where
  choice = choose
