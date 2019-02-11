{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( someFunc
  ) where

import Control.Lens
import Control.Monad
import Data.List
import Data.Map (Map, (!), update)
import qualified Data.Map as Map

type Cell = [Int]

type Grid = Map Int Cell

type Locus = [[Int]]

type Loci = Map Int Locus

data Sudoku = Sudoku
  { _grid :: Grid
  , _bindex :: Int
  , _misses :: Int
  , _hits :: Int
  , _deadends :: Int
  , _solutions :: [Grid]
  }

makeLenses ''Sudoku

{-
    Positions of values in the sudoku are by index 0..80:
     0  1  2 |  3  4  5 |  6  7  8
     9 10 11 | 12 13 14 | 15 16 17
    18 19 20 | 21 22 23 | 24 25 26
    ---------+----------+---------
    27 28 29 | 30 31 32 | 33 34 35
    36 37 38 | 39 40 41 | 42 43 44
    45 46 47 | 48 49 50 | 51 52 53
    ---------+----------+---------
    54 55 56 | 57 58 59 | 60 61 62
    63 64 65 | 66 67 68 | 69 70 71
    72 73 74 | 75 76 77 | 78 79 80
-}

-- SOLVED
-- "world's hardest sudoku" solves in about 8 seconds on my macbook
e = [[8],[],[],[],[],[],[],[],[],
     [],[],[3],[6],[],[],[],[],[],
     [],[7],[],[],[9],[],[2],[],[],
     [],[5],[],[],[],[7],[],[],[],
     [],[],[],[],[4],[5],[7],[],[],
     [],[],[],[1],[],[],[],[3],[],
     [],[],[1],[],[],[],[],[6],[8],
     [],[],[8],[5],[],[],[],[1],[],
     [],[9],[],[],[],[],[4],[],[]]

{-
  $ time stack exec sudoku-exe
  puzzle:
  8    |     |
      3|6    |
    7  |  9  |2
  -----+-----+-----
    5  |    7|
       |  4 5|7
       |1    |  3
  -----+-----+-----
      1|     |  6 8
      8|5    |  1
    9  |     |4
  1 solution in 11906 misses with 6431 hits and 87 deadends
  solution 1:
  8 1 2|7 5 3|6 4 9
  9 4 3|6 8 2|1 7 5
  6 7 5|4 9 1|2 8 3
  -----+-----+-----
  1 5 4|2 3 7|8 9 6
  3 6 9|8 4 5|7 2 1
  2 8 7|1 6 9|5 3 4
  -----+-----+-----
  5 2 1|9 7 4|3 6 8
  4 3 8|5 2 6|9 1 7
  7 9 6|3 1 8|4 5 2

  real    0m7.713s
  user    0m15.812s
  sys     0m3.234s
-}

-- another sudoku to try -- solves in about a second
--e = [[8],[],[],[],[3],[],[],[],[1],
--      [],[],[],[2],[],[8],[],[],[],
--      [],[],[7],[],[5],[],[6],[],[],
--      [],[3],[],[],[],[],[],[5],[],
--      [6],[],[8],[],[7],[],[2],[],[9],
--      [],[1],[],[],[],[],[],[6],[],
--      [],[],[9],[],[8],[],[5],[],[],
--      [],[],[],[1],[],[4],[],[],[],
--      [4],[],[],[],[6],[],[],[],[7]]

-- template to enter your own sudoku
--e = [[],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[],
--     [],[],[],[],[],[],[],[],[]]

-- given a particular index, the nine indexes of my row
row :: Int -> [Int]
row i = take 9 [i - mod i 9 ..]

-- the nine indexes of my column
col :: Int -> [Int]
col i = take 9 [x * 9 + mod i 9 | x <- [0 ..]]

-- the nine indexes of my box
box :: Int -> [Int]
box i =
  take 9 [x + y | x <- [i - mod i 27 + mod i 9 - mod i 3 ..], y <- [0, 9, 18]]

-- the three groups of eight indexes that concern me
eights :: Int -> [[Int]]
eights i = [delete i (f i) | f <- [row, col, box]]

subs :: [Int] -> [[Int]]
subs es = filter (not . null) (subsequences es)

-- the sets of indexes that can tell me about myself
mylocus :: Int -> Locus
mylocus i = concatMap subs (eights i)

myloci :: Loci
myloci = Map.fromList [(i, mylocus i) | i <- [0 .. 80]]

vals :: Grid -> [Int] -> [Int]
vals grid xs = nub $ concatMap (\x -> grid ! x) xs

strikes :: Grid -> Locus -> [Int]
strikes grid loc =
  nub $ concat [y | xs <- loc, let y = vals grid xs, length xs == length y]

prune4 :: Sudoku -> Int -> Bool -> Cell -> [Int] -> Sudoku
prune4 s n dirty cell xell
  | cell /= xell =
    prune2
      ((s & grid %~ Map.update (\y -> Just xell) n) & hits %~ (+ 1))
      (n + 1)
      True
  | otherwise = prune2 (s & misses %~ (+ 1)) (n + 1) dirty

prune3 :: Sudoku -> Int -> Bool -> Cell -> Sudoku
prune3 s n dirty cell
  | length cell > 1 =
    prune4 s n dirty cell (cell \\ strikes (s ^. grid) (myloci ! n))
  | otherwise = prune2 s (n + 1) dirty

prune2 :: Sudoku -> Int -> Bool -> Sudoku
prune2 s n dirty
  | n == 81 && dirty = prune2 s 0 False
  | n == 81 = s
  | otherwise = prune3 s n dirty ((s ^. grid) ! n)

prune :: Sudoku -> Sudoku
prune s = prune2 s 0 False

-- find an index with multiple values
branch :: Grid -> Int -> Maybe Int
branch grid bindex = find (\x -> 1 < length (grid ! x)) [bindex .. 80]

-- split into multiple solutions at the given index
split :: Grid -> Int -> [Grid]
split grid i = [Map.update (\_ -> Just [v]) i grid | v <- grid ! i]

splitter :: Sudoku -> Sudoku
splitter s =
  case branch (s ^. grid) (s ^. bindex) of
    Just i ->
      Data.List.foldl' (\a b -> solver (a & grid .~ b)) (s & bindex .~ (i + 1)) (split (s ^. grid) i)
    Nothing -> s

checker :: Sudoku -> Sudoku
checker s
  | all ((1 ==) . length) (s ^. grid) = s & solutions %~ (\a -> s ^. grid : a)
  | any ((0 ==) . length) (s ^. grid) = s & deadends %~ (+ 1)
  | otherwise = splitter s

solver :: Sudoku -> Sudoku
solver s = checker $ prune s

solve :: Grid -> Sudoku
solve grid = solver (Sudoku grid 0 0 0 0 [])

-- fill empty indexes of the initial sudoku input
fill :: [[Int]] -> Grid
fill vss =
  Map.fromList $
  zip
    [0 ..]
    [ case vs of
      [] -> [1 .. 9]
      _ -> vs
    | vs <- vss
    ]

-- below here is all formatting for printing

label :: Int -> String
label n = "solution " ++ show n ++ ":\n"

pv :: Cell -> String
pv vs =
  case vs of
    [n] -> show n
    _ -> " "

pd :: Int -> String
pd i
  | i < 80 && mod i 27 == 26 = "\n-----+-----+-----\n"
  | mod i 9 == 8 = "\n"
  | mod i 3 == 2 = "|"
  | otherwise = " "

pp :: String -> Grid -> String
pp prefix sol =
  Data.List.foldl' (++) prefix [pv (sol ! i) ++ pd i | i <- [0 .. 80]]

pn :: (String, String) -> Int -> String
pn t n
  | n == 1 = show n ++ " " ++ fst t
  | otherwise = show n ++ " " ++ snd t

banner :: Sudoku -> String
banner s =
  pn ("solution", "solutions") (length (view solutions s)) ++
  " in " ++
  pn ("miss", "misses") (view misses s) ++
  " with " ++
  pn ("hit", "hits") (view hits s) ++
  " and " ++ pn ("deadend", "deadends") (view deadends s) ++ "\n"

report :: Sudoku -> IO ()
report s =
  sequence_
    (putStr (banner s) :
     [ putStr x
     | x <-
         Data.List.map
           (\t -> pp (label (fst t)) (snd t))
           (zip [1 ..] (view solutions s))
     ])

-- called by app main
someFunc :: IO ()
someFunc = do
  let grid = fill e
  putStr (pp "puzzle:\n" grid)
  report (solve grid)
