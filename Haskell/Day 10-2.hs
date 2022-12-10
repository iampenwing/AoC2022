-- AdventOfCode 2022 Day 10, Puzzle 1
-- Rope Bridge
-- https://adventofcode.com/2022/day/9
-- Finds "badly packed" items

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import Data.List.Split
import System.Environment
import AoCLib.AoC2022 as AoC
import qualified Data.Set as Set

data Instruction = AddX Int | NoOp deriving (Show, Eq)

parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions (instruction:instructions)
  | (head instruction) == 'n' = NoOp:(parseInstructions instructions)
  | (head instruction) == 'a' = (AddX ((read (drop 5 instruction))::Int)):(parseInstructions instructions)

runInstructions :: [Instruction] -> Int -> [Int]
runInstructions [] n = [n]
runInstructions (instruction:instructions) n
  | instruction == NoOp = n:(runInstructions instructions n)
  | otherwise           = n:(n:(runInstructions instructions newN))
    where newN = n + (getArgument instruction)

getArgument :: Instruction -> Int
getArgument (AddX x) = x
getArgument NoOp = 0

getSpecialArguments :: [Int] -> [Int] -> [Int]
getSpecialArguments [] _ = []
getSpecialArguments (x:xs) regValues = (regValues!!(x-1)):(getSpecialArguments xs regValues)

drawCRT :: [[Int]] -> [[Char]]
drawCRT [] = []
drawCRT (x:xs) = (drawCRTLine 0 x):(drawCRT xs)

drawCRTLine :: Int -> [Int] -> [Char]
drawCRTLine _ [] = []
drawCRTLine n (x:xs) 
  | x >= (n - 1) && x <= (n + 1) = '#':(drawCRTLine (n + 1) xs)
  | otherwise                    = '.':(drawCRTLine (n + 1) xs)

drawCRTLines :: [String] -> String
drawCRTLines [] = []
drawCRTLines (x:xs) = x ++ ('\n':(drawCRTLines xs))

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let { instructions = parseInstructions . lines $ fileContents ;
        regValues = runInstructions instructions 1 ;
        specialValues = [20, 60, 100, 140, 180, 220] }
    in putStrLn (drawCRTLines (drawCRT (chunksOf 40 regValues)))

