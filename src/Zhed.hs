module Zhed where

-- base
import           Prelude ()
import           Data.Char          (digitToInt, isDigit)
import           Data.Foldable      (foldl')
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Traversable   (for)

-- sat-for-games
import           Ersatz.Prelude
import           Count    (Count, addBit)
import           Select   (Select, runSelect, selectPermutationN, selectList)
import           TotalMap (TotalMap, fromList, assign, lookup)

-- | Grid coordinate
data Coord = C Int Int -- ^ column row
  deriving (Read, Show, Ord, Eq)

-- | Cardinal directions: up, down, left, right
data Dir = U | D | L | R
  deriving (Read, Show, Ord, Eq)

-- | Board dimensions
data Dim = Dim { width, height :: Int }
  deriving (Read, Show, Ord, Eq)

------------------------------------------------------------------------

-- | Puzzle parameters: board dimensions, numbered cells, target cells
data Puzzle = Puzzle Dim [(Coord,Int)] [Coord]

exampleZhed :: Puzzle
exampleZhed = parsePuzzle
  "..2.....\n\
  \.......4\n\
  \2.....1.\n\
  \...1....\n\
  \....X...\n\
  \4.......\n\
  \3.......\n\
  \....1...\n"

smallZhed :: Puzzle
smallZhed = parsePuzzle
  "..2\n\
  \2..\n\
  \..X\n"

------------------------------------------------------------------------

cellsFromList :: Boolean a => [Coord] -> TotalMap Coord a
cellsFromList xs = TotalMap.fromList false [(x,true) | x <- xs]


coordsList :: Dim -> Coord -> Dir -> [Coord]
coordsList dim (C x y) dir =
  case dir of
    U -> [ C x i | i <- [ y-1, y-2 .. 0              ] ]
    L -> [ C i y | i <- [ x-1, x-2 .. 0              ] ]
    D -> [ C x i | i <- [ y+1, y+2 .. height dim - 1 ] ]
    R -> [ C i y | i <- [ x+1, x+2 .. width  dim - 1 ] ]


applyMove ::
  Dim                      {- ^ board dimensions -} ->
  TotalMap Coord Bit       {- ^ filled cells     -} ->
  Select (Coord, Int, Dir) {- ^ chosen move      -} ->
  TotalMap Coord Bit       {- ^ updated cells    -}
applyMove dim cells move =
  runSelect $
    do (start, len, dir) <- move
       let (_,cells') = foldl'
                          (spreadCell len)
                          (encode 0, cells)
                          (coordsList dim start dir)
       return cells'


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


solutionExists ::
  Puzzle ->
  Ersatz [Select (Coord, Int, Dir)]
solutionExists (Puzzle dim squares targets) =

  do -- Choose an order for the solution sequence
     order <- selectPermutationN (length squares) squares

     -- Choose a direction for each square in the solution sequence
     orderDirs <-
       for order $ \s ->
         do d <- selectList (U:|[D,L,R])
            return $ do (x,y) <- s
                        z     <- d
                        return (x,y,z)

     -- compute covered cells after applying the chosen moves
     let board = foldl' (applyMove dim)
                        (cellsFromList [ c | (c,_) <- squares ])
                        orderDirs

     -- Check that at least one target is covered
     assert (any (\target -> TotalMap.lookup target board) targets)

     return orderDirs


findSolution :: Puzzle -> IO (Maybe [(Coord, Int, Dir)])
findSolution = solve . solutionExists


zhed :: Puzzle -> IO ()
zhed puzzle =
  do result <- findSolution puzzle
     case result of
       Nothing -> putStrLn "No solution"
       Just solution ->
         putStr (renderSolution puzzle solution)

------------------------------------------------------------------------

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
