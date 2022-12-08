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
  

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let visibleForest = countVisible (map makeVisible (checkAllDirections (makeForest (lines (fileContents)))))
      in putStrLn (show visibleForest)



