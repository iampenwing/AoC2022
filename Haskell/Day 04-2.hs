-- AdventOfCode 2022 Day 3, Puzzle 1, Version 2 SETS
-- Rucksack Reorganisation
-- https://adventofcode.com/2022/day/3
-- Finds "badly packed" items

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import Data.List.Split
import System.Environment
import AoCLib.AoC2022 as AoC
import qualified Data.Set as Set

parseLine :: String -> (Set.Set Int, Set.Set Int)
parseLine s = let [[aStart, aEnd], [bStart, bEnd]] = map (splitOn "-") . splitOn "," $ s
              in (Set.fromList [(myReadInt aStart)..(myReadInt aEnd)], Set.fromList [myReadInt(bStart)..(myReadInt bEnd)])

checkOverlaps :: (Set.Set Int, Set.Set Int) -> Int
checkOverlaps (a, b) 
  | Set.null (a `Set.intersection` b) = 0
  | otherwise                         = 1

                
main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = foldl (+) 0 . map checkOverlaps . map parseLine . lines $ fileContents
         in putStrLn (show answer)
