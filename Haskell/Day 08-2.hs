-- Advent of Code 2022 - Day 7 Puzzle 1
-- Alex Lambert
-- aoc@penwing.me.uk

import Data.List
import Data.List.Split
import System.Environment
import AoCLib.AoC2022 as AoC

checkFromLeft :: [[Int]] -> [[Bool]]
checkFromLeft [] = []
checkFromLeft (line:lines) = (checkFromLeftHelper (-1) line):(checkFromLeft lines)

checkFromLeftHelper :: Int -> [Int] -> [Bool]
checkFromLeftHelper _ [] = []
checkFromLeftHelper n (t:trees)
  | n >= t     = True:(checkFromLeftHelper n trees)
  | otherwise = False:(checkFromLeftHelper t trees)

checkFromRight :: [[Int]] -> [[Bool]]
checkFromRight forest = map reverse (checkFromLeft (map reverse forest))

checkFromTop :: [[Int]] -> [[Bool]]
checkFromTop forest = transpose (checkFromLeft (transpose forest))

checkFromBottom :: [[Int]] -> [[Bool]]
checkFromBottom forest = transpose (checkFromRight (transpose forest))

mergeBoolArray :: (Bool -> Bool -> Bool) -> [[Bool]] -> [[Bool]] -> [[Bool]]
mergeBoolArray _ [] _ = []
mergeBoolArray func (arrayAHead:arrayARest) (arrayBHead:arrayBRest) = (zipWith func arrayAHead arrayBHead):(mergeBoolArray func arrayARest arrayBRest)

checkAllDirections :: [[Int]] -> [[Bool]]
checkAllDirections [] = []
checkAllDirections forest = mergeBoolArray (&&) (mergeBoolArray (&&) (mergeBoolArray (&&) (checkFromLeft forest) (checkFromRight forest)) (checkFromTop forest)) (checkFromBottom forest)

countVisible :: [[Int]] -> Int
countVisible [] = 0
countVisible forest = foldr (+) 0 (map (\x -> foldr (+) 0 x) forest)

makeVisible :: [Bool] -> [Int]
makeVisible [] = []
makeVisible (x:xs)
  | x         = 0:(makeVisible xs)
  | otherwise = 1:(makeVisible xs)

makeForest :: [String] -> [[Int]]
makeForest [] = []
makeForest (line:lines) = (map makeHeight line):(makeForest lines)

makeHeight :: Char -> Int
makeHeight tree
  | tree == '0' = 0
  | tree == '1' = 1
  | tree == '2' = 2
  | tree == '3' = 3
  | tree == '4' = 4
  | tree == '5' = 5
  | tree == '6' = 6
  | tree == '7' = 7
  | tree == '8' = 8
  | tree == '9' = 9
  

lookHelper :: Int -> [Int] -> Int
lookHelper _ [] = 0
lookHelper n (tree:trees)
  | n <= tree = 1
  | otherwise = 1 + (lookHelper n trees)

lookRight :: [Int] -> [Int]
lookRight [] = []
lookRight (tree:trees) = (lookHelper tree trees):(lookRight trees)

lookRights :: [[Int]] -> [[Int]]
lookRights forest = map lookRight forest

lookLeft :: [Int] -> [Int]
lookLeft trees = reverse (lookRight (reverse trees))

lookLefts :: [[Int]] -> [[Int]]
lookLefts forest = map lookLeft forest

lookDowns :: [[Int]] -> [[Int]]
lookDowns forest = transpose (map lookRight (transpose forest))

lookUps :: [[Int]] -> [[Int]]
lookUps forest = transpose (map lookLeft (transpose forest))

zipLooks :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
zipLooks [] _ _ _ = []
zipLooks _ [] _ _ = []
zipLooks _ _ [] _ = []
zipLooks _ _ _ [] = []
zipLooks (up:ups) (down:downs) (left:lefts) (right:rights) = (zipLooksHelper up down left right):(zipLooks ups downs lefts rights)

zipLooksHelper :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
zipLooksHelper up down left right = (zipWith (*) left (zipWith (*) right (zipWith (*) up down)))

visibilityScores :: [[Int]] -> [[Int]]
visibilityScores forest = let left = lookLefts forest
                              right = lookRights forest
                              up = lookUps forest
                              down = lookDowns forest
                              in zipLooks up down left right

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let visibleForest = (makeForest (lines (fileContents)))
      in putStrLn (show (maximum (map (maximum) (visibilityScores visibleForest))))


