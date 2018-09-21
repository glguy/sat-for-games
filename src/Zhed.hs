{-|
Module      : Zhed
Description : Solver for Zhed puzzle
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

This module implements a solver for the Zhed puzzle game.

In Zhed the player is presented with a grid of cells that can be either
marked or unmarked. Additionally one or more cells will be designated
as the target cell. In the initial puzzle state each of the pre-marked
cells will have a number on it. Each of these numbered cells can be activated
exactly once. When activated a cardinal direction is chosen. The number
indicates how many of the nearest, unmarked cells will become marked.

The puzzle is considered solved when a target cell is marked.

https://play.google.com/store/apps/details?id=com.groundcontrol.zhed&hl=en_US

-}
module Zhed where

-- base
import           Prelude ()
import           Control.Monad      (replicateM)
import           Data.Char          (digitToInt, isDigit)
import           Data.Foldable      (foldl')
import           Data.List.NonEmpty (NonEmpty((:|)))

-- sat-for-games
import           Ersatz.Prelude
import           Count    (Count, addBit)
import           Select   (Select, runSelect, selectPermutationN, selectList)
import           TotalMap (TotalMap, fromList, assign, lookup)

-- | Grid coordinate
data Coord = C Int Int -- ^ column row
  deriving (Read, Show, Ord, Eq)

-- | Cardinal directions: up, down, left, right
data Dir
  = U -- ^ up
  | D -- ^ down
  | L -- ^ left
  | R -- ^ right
  deriving (Read, Show, Ord, Eq)

-- | Board dimensions
data Dim = Dim { width, height :: Int }
  deriving (Read, Show, Ord, Eq)

------------------------------------------------------------------------

-- | Puzzle parameters: board dimensions, numbered cells, target cells
data Puzzle = Puzzle Dim [(Coord,Int)] [Coord]
  deriving (Read, Show, Ord, Eq)

-- | An 8-by-8 puzzle
largeZhed :: Puzzle
largeZhed = parsePuzzle
  "..2.....\n\
  \.......4\n\
  \2.....1.\n\
  \...1....\n\
  \....X...\n\
  \4.......\n\
  \3.......\n\
  \....1...\n"

-- | A 3-by-3 puzzle
smallZhed :: Puzzle
smallZhed = parsePuzzle
  "..1\n\
  \2..\n\
  \..X\n"

------------------------------------------------------------------------

-- | Construct a mapping from coordinates to bits suitable for use as a
-- symbolic set.
cellsFromList ::
  [Coord]            {- ^ coordinate list                 -} ->
  TotalMap Coord Bit {- ^ map listed coordinates set true -}
cellsFromList xs = TotalMap.fromList false [(x,true) | x <- xs]


-- | Produce a list of coordinates generated starting at a given
-- location, traveling in a given direction, until the edge of
-- the board is reached. The starting location is not included.
coordsList ::
  Dim     {- ^ board dimensions                      -} ->
  Coord   {- ^ starting coordinate                   -} ->
  Dir     {- ^ direction                             -} ->
  [Coord] {- ^ list of coordinates in that direction -}
coordsList dim (C x y) dir =
  case dir of
    U -> [ C x i | i <- [ y-1, y-2 .. 1          ] ]
    L -> [ C i y | i <- [ x-1, x-2 .. 1          ] ]
    D -> [ C x i | i <- [ y+1, y+2 .. height dim ] ]
    R -> [ C i y | i <- [ x+1, x+2 .. width  dim ] ]


-- | Given a board's dimensions and the map of cells, update
-- the cell map with a symbolically chosen square activation.
applyMove ::
  Dim                      {- ^ board dimensions            -} ->
  TotalMap Coord Bit       {- ^ filled cells                -} ->
  Select (Coord, Int, Dir) {- ^ chosen square and direction -} ->
  TotalMap Coord Bit       {- ^ updated cells               -}
applyMove dim cells move =
  runSelect $
    do (start, len, dir) <- move
       let (_,cells') = foldl'
                          (spreadCell len)
                          (encode 0, cells)
                          (coordsList dim start dir)
       return cells'


-- | Attempt to mark a particular cell on the given board. Only
-- mark a cell if there the count of cells placed is less than
-- the total number available.
spreadCell ::
  Int                         {- ^ squares available to place        -} ->
  (Count, TotalMap Coord Bit) {- ^ squares placed and current board  -} ->
  Coord                       {- ^ coordinate to place on            -} ->
  (Count, TotalMap Coord Bit) {- ^ updated placement count and board -}
spreadCell len (used, cells) coord = (used', cells')
  where
    old = TotalMap.lookup coord cells
    new = old || used <? encode len

    used'  = addBit used (not old)
    cells' = TotalMap.assign coord new cells


-- | Combine the choice of square to activate with a direction to
-- get back a single choice containing both.
combineChoices :: Select (Coord, Int) -> Select Dir -> Select (Coord, Int, Dir)
combineChoices selectSq selectDir =
  do (coord, n) <- selectSq
     dir        <- selectDir
     return (coord, n, dir)


-- | Generate a solution that satisfies the clues in the given puzzle.
-- The solution is represented as an ordered list of the squares to
-- activate and the order to activate them in.
solutionExists ::
  Puzzle                            {- ^ puzzle parameters -} ->
  Ersatz [Select (Coord, Int, Dir)] {- ^ puzzle solution   -}
solutionExists (Puzzle dim squares targets) =

  do let n = length squares

     -- Choose an order to active the squares
     squares' <- selectPermutationN n squares

     -- Choose an order to active the squares
     dirs <- replicateM n (selectList (U:|[D,L,R]))

     let steps = zipWith combineChoices squares' dirs

     let initialCells = cellsFromList [c | (c,_) <- squares]

     -- compute covered cells after applying the chosen moves
     let finalCells   = foldl' (applyMove dim) initialCells steps

     -- Check that at least one target is covered
     assert (any (`TotalMap.lookup` finalCells) targets)

     return steps


-- | Run the SAT solver on the solution generated by 'solutionExists'
findSolution :: Puzzle -> IO (Maybe [(Coord, Int, Dir)])
findSolution = solve . solutionExists


-- | Solve a given Zhed puzzle and print the solution to stdout.
zhed :: Puzzle -> IO ()
zhed puzzle =
  do result <- findSolution puzzle
     case result of
       Nothing -> putStrLn "No solution"
       Just solution ->
         putStr (renderSolution puzzle solution)

------------------------------------------------------------------------

-- | Parse a puzzle representing square by their number value,
-- targets with an @X@ and empty space with @.@
--
-- Example
--
-- @
-- ..1
-- 2..
-- ..X
-- @
parsePuzzle :: String -> Puzzle
parsePuzzle input = Puzzle (Dim w h) numbers targets
  where
    rows = lines input
    w    = maximum (0 : map length rows)
    h    = length rows

    cells = [(C x y, elt) | (y,row) <- zip [1..] rows
                          , (x,elt) <- zip [1..] row]

    numbers = [(xy, digitToInt n) | (xy, n ) <- cells, isDigit n]
    targets = [xy                 | (xy,'X') <- cells]


-- | Render a puzzle and its solution. Each square in the
-- puzzle will be changed to be its sequence number in
-- a satisfying solution and an indicator of the direction
-- in which that square should be expanded.
renderSolution ::
  Puzzle              {- ^ puzzle            -} ->
  [(Coord, Int, Dir)] {- ^ solution order    -} ->
  String              {- ^ rendered solution -}
renderSolution (Puzzle dim squares targets) steps
  = mapToString
  $ stage addTarget targets
  $ stage addStep   steps'
  $ stage addSquare squares
  $ TotalMap.fromList " . " []

  where
    stage f xs m = foldr f m xs

    -- replace square number with sequence number
    steps' = [ (xy, i, dir) | (i, (xy, _, dir)) <- zip [1 :: Int ..] steps]

    addTarget xy         = TotalMap.assign xy " X "
    addSquare (xy,_)     = TotalMap.assign xy " # "
    addStep (xy, n, dir) = TotalMap.assign xy (pad ++ show n ++ showDir dir)
      where
        pad = if n < 10 then " " else ""

    showDir d = case d of U -> "^"; D -> "v"; L -> "<"; R -> ">"

    mapToString m =
      unlines [ concat [ TotalMap.lookup (C x y) m
                       | x <- [1 .. width dim] ]
              | y <- [1 .. height dim] ]
