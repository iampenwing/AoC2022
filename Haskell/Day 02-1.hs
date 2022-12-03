-- AdventOfCode 2022 Day 2, Puzzle 1
-- Rock Paper Scissors
-- https://adventofcode.com/2022/day/2
-- Tournament Scoring

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
scoreLine "A X" = rock + draw
scoreLine "A Y" = paper + win
scoreLine "A Z" = scissors + loss
scoreLine "B X" = rock + loss
scoreLine "B Y" = paper + draw
scoreLine "B Z" = scissors + win
scoreLine "C X" = rock + win
scoreLine "C Y" = paper + loss
scoreLine "C Z" = scissors + draw

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = (foldr (+) 0 (map scoreLine (lines fileContents)))
         in putStrLn (show answer)
