-- AdventOfCode 2022 Day 3, Puzzle 1, Version 2 SETS
-- Rucksack Reorganisation
-- https://adventofcode.com/2022/day/3
-- Finds "badly packed" items

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import AoCLib.AoC2022 as AoC

day3Task1Score :: Char -> Int
day3Task1Score c =
  let i = (fromEnum c)
  in if (i < 91)
     then (i - 38)
     else (i - 96)
            
main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = (foldr (+) 0 (map (day3Task1Score) (map (AoC.findCommon) (map (AoC.packRucksack) (lines fileContents)))))
         in putStrLn (show answer)
