-- AdventOfCode 2022 Day 1, Puzzle 1
-- Calorie Counting
-- https://adventofcode.com/2022/day/1
-- Counts when depth increases

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment

myReadInt :: [Char] -> Int
myReadInt ('+':xs) = read xs ::Int
myReadInt x = read x :: Int

countLists :: [String] -> [Int]
countLists [] = []
countLists (x:xs) =
  let (topCount, rest) = (iCountLists 0 (x:xs))
  in (topCount:(countLists rest))

iCountLists :: Int -> [String] -> (Int, [String])
iCountLists count [] = (count, [])
iCountLists count ("":xs) = (count, xs)
iCountLists count (x:xs) = iCountLists (count + (myReadInt x)) xs

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = (maximum (countLists (lines fileContents)))
         in putStrLn (show answer)
