-- AdventOfCode 2022 Day 6, Puzzle 2
-- Tuning Trouble
-- https://adventofcode.com/2022/day/6
-- Finds "badly packed" items

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import Data.List.Split
import System.Environment
import AoCLib.AoC2022 as AoC

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  putStrLn (show (findUniqueSequence 14 topCrates) + 14)
