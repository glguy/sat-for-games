{-# Language GeneralizedNewtypeDeriving #-}
{-|
Module      : Ersatz.Prelude
Description : Single module for providing Prelude and Ersatz
              definitions.
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

This module makes it easier to use Ersatz by reëxporting Prelude
without collisions with Ersatz, and by providing a distinct type
for building SAT problems instead of relying on MTL classes.

Define your problem using 'exists' and 'assert' and then invoke
the SAT solver on the resulting expression with 'solve'.

>>> :{
solve $
  do x <- existsBits 4
     y <- existsBits 4
     assert (x + y === 16)
     assert (x * y === 55)
     assert (x <=? y)
     return (x,y)
:}
Just (5,11)

-}
module Ersatz.Prelude
  (
  -- * modules
    module Ersatz
  , module Prelude

  -- * types
  , Ersatz

  -- * operations
  , exists
  , assert
  , solve
  , runErsatz

  -- * variable-width number operations
  , existsBits
  ) where

-- base
import           Prelude hiding (and, or, any, all, (&&), (||), not)
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (MonadIO)

-- ersatz
import           Ersatz (Boolean(..), Equatable(..), Orderable(..),
                         Bit, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7, Bit8,
                         Bits(..),
                         SAT, Variable, Codec(..))
import qualified Ersatz as E

-- transformers
import           Control.Monad.Trans.State (StateT)


-- | Computations that can quantify new variables, generate assertions,
-- and return results.
newtype Ersatz a = Ersatz (StateT SAT IO a)
  deriving (Functor, Applicative, Monad, MonadIO)


-- | Generates a fresh, existentially quantified variable.
--
-- Specialization of 'E.exists'
exists :: Variable a => Ersatz a
exists = Ersatz E.exists


-- | Add a new assertion to be satisfied by the solver.
--
-- Specialization of 'E.assert'
assert :: Bit -> Ersatz ()
assert p = Ersatz (E.assert p)


-- | Given an Ersatz problem value, compute a model that
-- satisfies all of the assertions and return the decoded
-- result, or return 'Nothing' to indicate that no model
-- is possible.
--
-- Specialization of 'E.solveWith'
solve :: Codec a => Ersatz a -> IO (Maybe (Decoded a))
solve (Ersatz problem) =
  do result <- E.solveWith E.minisat problem
     case result of
       (E.Satisfied, Just solution) -> return (Just solution)
       (E.Unsatisfied, _          ) -> return Nothing
       _                            -> fail "solve: PANIC"


-- | Generate a fresh number with a given number of bits.
existsBits :: Int {- ^ bits -} -> Ersatz Bits
existsBits n = Bits <$> replicateM n exists


-- | Unwrap an Ersatz problem value computing the returned
-- value and SAT expression. While this can be instructive,
-- it is more common and useful to use 'solve'.
runErsatz :: Ersatz a -> IO (a, SAT)
runErsatz (Ersatz e) = E.runSAT e
