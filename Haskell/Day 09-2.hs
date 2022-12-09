-- AdventOfCode 2022 Day 9, Puzzle 1
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


data Instruction = InsUp | InsDown | InsLeft | InsRight deriving (Show, Eq)

parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions ((i:(' ':count)):instructions) = let n = (read (count))::Int
                                                   in (parseInstruction i n) ++ parseInstructions instructions


parseInstruction :: Char -> Int -> [Instruction]
parseInstruction instruction 1
  | instruction == 'U' = [InsUp]
  | instruction == 'D' = [InsDown]
  | instruction == 'L' = [InsLeft]
  | instruction == 'R' = [InsRight]
parseInstruction instruction n
  | instruction == 'U' = InsUp:(parseInstruction instruction (n - 1))
  | instruction == 'D' = InsDown:(parseInstruction instruction (n - 1))
  | instruction == 'L' = InsLeft:(parseInstruction instruction (n - 1))
  | instruction == 'R' = InsRight:(parseInstruction instruction (n - 1))

moveHead :: (Int, Int) -> Instruction -> (Int, Int)
moveHead (x, y) i
  | i == InsUp    = (x, (y + 1))
  | i == InsDown  = (x, (y - 1))
  | i == InsLeft  = ((x - 1), y)
  | i == InsRight = ((x + 1), y)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (tailX, tailY) (headX, headY)
  -- Touching
  | (tailX >= (headX - 1)) && (tailX <= (headX + 1)) && (tailY >= (headY - 1)) && (tailY <= (headY + 1)) = (tailX, tailY)
  -- same column
  | (tailY == headY) && (tailX < headX) = ((tailX + 1), tailY)
  | (tailY == headY) && (tailX > headX) = ((tailX - 1), tailY)
  -- same row
  | (tailX == headX) && (tailY < headY) = (tailX, (tailY + 1))
  | (tailX == headX) && (tailY > headY) = (tailX, (tailY - 1))
  -- diagonals
  | (tailX < headX) && (tailY < headY) = ((tailX + 1), (tailY + 1))
  | (tailX < headX) && (tailY > headY) = ((tailX + 1), (tailY - 1))
  | (tailX > headX) && (tailY < headY) = ((tailX - 1), (tailY + 1))
  | (tailX > headX) && (tailY > headY) = ((tailX - 1), (tailY - 1))
 
executeInstruction :: Instruction -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
executeInstruction i headOfRope tailOfRope = let newHeadOfRope = moveHead headOfRope i
                                             in (newHeadOfRope, (moveTail tailOfRope newHeadOfRope))


doInstructions :: [Instruction] -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
doInstructions [] _ _ visited = visited
doInstructions (i:is) ropeHead ropeTail visited = let (newHead, newTail) = executeInstruction i ropeHead ropeTail
                                                  in doInstructions is newHead newTail (Set.insert newTail visited)

testData = ["R 4","U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]
testInstructions = parseInstructions testData

quickBuildInstruction :: [Instruction] -> (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))]
quickBuildInstruction [] ropeHead ropeTail = [(ropeHead, ropeTail)]
quickBuildInstruction (i:ins) ropeHead ropeTail = let (newHead, newTail) = executeInstruction i ropeHead ropeTail
                                                  in (ropeHead, ropeTail):(quickBuildInstruction ins newHead newTail)

getFinalElement :: [a] -> a
getFinalElement (x:[]) = x
getFinalElement (_:xs) = getFinalElement xs

quickCalcFinalHead :: [Instruction] -> (Int, Int)
quickCalcFinalHead [] = (0,0)
quickCalcFinalHead xs = let ups = length (filter (\x -> x == InsUp) xs)
                            downs = length (filter (\x -> x == InsDown) xs)
                            lefts = length (filter (\x -> x == InsLeft) xs)
                            rights = length (filter (\x -> x == InsRight) xs)
                            finalY = ups - downs
                            finalX = rights - lefts
                        in (finalX, finalY)

chaseRope :: (Int, Int) -> [(Int, Int)] -> [(Int,Int)]
chaseRope x [] = [x]
chaseRope t (h:route) = newT:(chaseRope newT route)
  where newT = moveTail t h

chaseRopes :: Int -> [(Int, Int)] -> [(Int, Int)]
chaseRopes 0 route = route
chaseRopes x route = chaseRopes (x - 1) (chaseRope (0, 0) route)

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let { instructions = parseInstructions . lines $ fileContents ;
        tailRoute = map snd (quickBuildInstruction instructions (0, 0) (0, 0));
        lastTail = chaseRopes 8 tailRoute}
    in putStrLn (show (Set.size (Set.fromList lastTail)))
