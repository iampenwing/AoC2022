-- AdventOfCode 2022 Day 1, Puzzle 1
-- Calorie Counting
-- https://adventofcode.com/2022/day/1
-- Counts when depth increases

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import AoCLib.AoC2022 as AoC

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = (maximum (AoC.countLists (lines fileContents)))
         in putStrLn (show answer)
