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
-- import qualified Data.Set as Set

parseInitData :: Int -> [String] -> [String]
parseInitData 1 towers = (parseTower 0 towers):[]
parseInitData n towers = (parseTower (n - 1) towers):(parseInitData  (n - 1) towers)

parseTower :: Int -> [String] -> String
parseTower n towers = map (\x -> x!!(1 + (n * 4))) towers

cleanTower :: String -> String
cleanTower [] = []
cleanTower (x:xs)
  | x == ' ' = cleanTower xs
  | otherwise = (x:xs)

prepareMove :: String -> (Int, Int, Int)
prepareMove move =
  let [_, crates, _, tower1, _, tower2] = splitOn " " move
  in ((read crates)::Int, (read tower1)::Int, (read tower2)::Int)

runMoves :: [String] -> [(Int, Int, Int)] -> [String]
runMoves towers [] = towers
runMoves towers ((crates, tower1, tower2):moves) = runMoves (moveCrates crates tower1 tower2 towers) moves

-- runMove :: [String] -> (Int, Int, Int) -> [String]
-- runMove (n, tower1, tower2) = 

-- runMove towers (0, _, _) = towers
-- runMove towers (n, tower1, tower2) =
--   let newTowers = moveCrates tower1 tower2 towers
--   in runMove newTowers ((n-1), tower1, tower2)

popCrates :: Int -> Int -> [String] -> ([Char], [String])
popCrates n 1 (tower:towers) = ((take n tower), ((drop n tower):towers))
popCrates n t (tower:towers) = let (crates, newTowers) = popCrates n (t - 1) towers
                               in (crates, (tower:newTowers))

pushCrates :: Int -> [Char] -> [String] -> [String]
pushCrates 1 c (tower:towers) = ((c ++ tower):towers)
pushCrates n c (tower:towers) = (tower:(pushCrates (n - 1) c towers))


moveCrates :: Int -> Int -> Int -> [String] -> [String]
moveCrates c x y towers = let (crates, newTowers) = popCrates c x towers
                        in pushCrates y crates newTowers

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let [initData, moveData] = splitOn [""] . lines $ fileContents
      initDataLength = (+ 1) . length . head $ initData 
      towersCount = initDataLength `div` 4
      initialState = reverse (map cleanTower (parseInitData towersCount (take ((length initData) - 1) initData)))
      preparedMoves = map prepareMove moveData
      towers = runMoves initialState preparedMoves
      topCrates = map (head) towers
         in putStrLn (show topCrates)
