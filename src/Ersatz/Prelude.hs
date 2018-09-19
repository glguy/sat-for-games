{-# Language GeneralizedNewtypeDeriving #-}
{- |
This module makes it easier to use Ersatz by reëxporting Prelude
without collisions with Ersatz, and by providing a distinct type
for building SAT problems instead of relying on class constraints.
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
  ) where

-- base
import           Prelude hiding (and, or, any, all, (&&), (||), not)
import           Control.Monad.IO.Class (MonadIO)

-- ersatz
import           Ersatz hiding (assert, exists, solveWith)
import qualified Ersatz as E

-- transformers
import           Control.Monad.Trans.State (StateT)


-- | Computations that can quantify new variables, generate assertions,
-- and return results.
newtype Ersatz a = Ersatz (StateT SAT IO a)
  deriving (Functor, Applicative, Monad)


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
  do result <- E.solveWith minisat problem
     case result of
       (Satisfied, Just solution) -> return (Just solution)
       (Unsatisfied, _          ) -> return Nothing
       _                          -> fail "solve: PANIC"


-- | Unwrap an Ersatz problem value computing the returned
-- value and SAT expression. While this can be instructive,
-- it is more common to use 'solve'.
runErsatz :: Ersatz a -> IO (a, SAT)
runErsatz (Ersatz e) = runSAT e
