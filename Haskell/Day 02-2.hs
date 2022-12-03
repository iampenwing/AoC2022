-- AdventOfCode 2022 Day 2, Puzzle 2
-- Rock Paper Scissors
-- https://adventofcode.com/2022/day/2
-- Correct Tournament Scoring

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import AoCLib.AoC2022 as AoC

rock = 1
paper = 2
scissors = 3

win = 6
draw = 3
loss = 0

scoreLine :: String -> Int
scoreLine "A X" = loss + scissors
scoreLine "A Y" = draw + rock
scoreLine "A Z" = win + paper
scoreLine "B X" = loss + rock
scoreLine "B Y" = draw + paper
scoreLine "B Z" = win + scissors
scoreLine "C X" = loss + paper
scoreLine "C Y" = draw + scissors
scoreLine "C Z" = win + rock

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = (foldr (+) 0 (map scoreLine (lines fileContents)))
         in putStrLn (show answer)
