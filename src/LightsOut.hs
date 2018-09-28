{-# Language QuasiQuotes #-}
{-|
Module      : LightsOut
Description : Solver for Lights Out puzzle
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

https://en.wikipedia.org/wiki/Lights_Out_(game)

This module implements a solver for the Lights Out game. The solver
works by encoding the lights out problem in terms of boolean satisfiability
using the Ersatz library.

This module does not attempt to produce a solution with the minimal number
of button presses. This enhancement is left as an exercise for the reader!

-}
module LightsOut where

-- base
import           Prelude () -- handled by Ersatz.Prelude

-- containers
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

-- sat-for-games
import           Ersatz.Prelude

------------------------------------------------------------------------

-- | Specification of a Lights Out puzzle
data LightsOut = LightsOut
  { dimensions :: Dim
  , lightsOn   :: Set Coord }
  deriving Show

-- | Example puzzle that spells ICFP.
exampleLightsOut :: LightsOut
exampleLightsOut = parsePuzzle [str|
  ***..***..***..**.
  .*...*....*....*.*
  .*...*....**...**.
  .*...*....*....*..
  ***..***..*....*..
|]

-- | Small 3-by-3 puzzle
smallLightsOut :: LightsOut
smallLightsOut = parsePuzzle [str|
  **.
  *.*
  .**
|]

-- | Example of a puzzle that has no solution.
unsolvableLightsOut :: LightsOut
unsolvableLightsOut = parsePuzzle [str|
  *....
  *....
  .....
  .....
  .....
|]

------------------------------------------------------------------------

-- | Grid coordinate
data Coord = C Int Int -- ^ column row
  deriving (Eq, Ord, Show, Read)

-- | Cardinal direction neighborhood for a coordinate. Results include
-- the given coordinate.
--
-- >>> neighborhood (C 2 3)
-- [C 2 2,C 1 3,C 2 3,C 3 3,C 2 4]
neighborhood ::
  Coord   {- ^ coordinate                     -} ->
  [Coord] {- ^ north, west, self, east, south -}
neighborhood (C x y) =
  [            C x (y-1)
  , C (x-1) y, C x y    , C (x+1) y
  ,            C x (y+1)]

------------------------------------------------------------------------

-- | Board dimensions
data Dim = Dim Int Int -- ^ width height
  deriving (Eq, Ord, Show, Read)

-- | Generate a list of all coordinates in a board of the given size.
--
-- >>> dimCoords (Dim 3 2)
-- [C 1 1,C 1 2,C 2 1,C 2 2,C 3 1,C 3 2]
dimCoords :: Dim -> [Coord]
dimCoords (Dim w h) = [C x y | x <- [1..w], y <- [1..h]]

------------------------------------------------------------------------

-- | Generate a map associating coordinates that can be toggled with
-- whether or not that coordinate was toggled.
existsSolution :: Dim -> Ersatz (Map Coord Bit)
existsSolution dim = sequenceA mapOfExists
  where
    mapOfExists :: Map Coord (Ersatz Bit)
    mapOfExists = Map.fromList [(c, exists) | c <- dimCoords dim]

------------------------------------------------------------------------

-- | Predicate for determining if a set of toggles results in all
-- lights being turned off. A clicked coordinate corresponds to an
-- entry in the given map with a 'true' value.
isValidSolution ::
  LightsOut     {- ^ puzzle              -} ->
  Map Coord Bit {- ^ clicked coordinates -} ->
  Bit           {- ^ solution is valid   -}
isValidSolution puzzle vars = allOff
  where
    allOff         = all (not . isOn) (Map.keys vars)

    initialState c = bool (c `Set.member` lightsOn puzzle)

    isClicked    c = Map.findWithDefault false c vars

    isOn         x = error "isValidSolution.isOn not implemented"

------------------------------------------------------------------------

-- | Find a solution to the given puzzle that also satisfies
-- an additional predicate if one exists.
--
-- >>> findSolution smallLightsOut
-- Just (fromList [(C 1 1,True),(C 1 2,False),(C 1 3,False),(C 2 1,False),(C 2 2,False),(C 2 3,False),(C 3 1,False),(C 3 2,False),(C 3 3,True)])
findSolution ::
  LightsOut                   {- ^ puzzle                 -} ->
  IO (Maybe (Map Coord Bool)) {- ^ solution if one exists -}
findSolution puzzle =
  solve $
    do soln <- existsSolution (dimensions puzzle)
       assert (isValidSolution puzzle soln)
       return soln


-- | Attempt to solve the given puzzle and print the solution to stdout.
--
-- >>> lightsOut smallLightsOut 
-- *..
-- ...
-- ..*
lightsOut ::
  LightsOut {- ^ puzzle                   -} ->
  IO ()     {- ^ print solution to puzzle -}
lightsOut puzzle =
  do result <- findSolution puzzle
     case result of
       Nothing -> putStrLn "No solution"
       Just solution ->
         putStr (renderSolution (dimensions puzzle) solution)


------------------------------------------------------------------------

-- | Parse a lights out puzzle encoded as lines of @.@ for off and
-- @*@ for on.
--
-- Example input
--
-- @
-- *.*.
-- .*..
-- ...*
-- @
--
-- >>> parsePuzzle "*.*.\n.*..\n...*\n"
-- LightsOut {dimensions = Dim 4 3, lightsOn = fromList [C 1 1,C 2 2,C 3 1,C 4 3]}
parsePuzzle :: String -> LightsOut
parsePuzzle input = LightsOut (Dim w h) (Set.fromList coords)
  where
    rows = lines input
    w    = maximum (0 : map length rows)
    h    = length rows

    coords = [ C x y | (y,row) <- zip [1..] rows
                     , (x,'*') <- zip [1..] row ]


-- | Render a lights out puzzle using @.@ and @*@ as in 'parsePuzzle'.
renderSolution ::
  Dim            {- ^ board dimensions  -} ->
  Map Coord Bool {- ^ solution map      -} ->
  String         {- ^ rendered solution -}
renderSolution (Dim w h) coords =
  unlines
    [ [ if coords Map.! C x y then '*' else '.'
      | x <- [1 .. w] ]
    |  y <- [1 .. h] ]
