-- AdventOfCode 2022 Day 11, Puzzle 1
-- Monkey Business
-- https://adventofcode.com/2022/day/11

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import Data.List.Split
import System.Environment
import AoCLib.AoC2022 as AoC
import qualified Data.Set as Set

data Monkey = Monkey { monkeyName :: Int
                     , items :: [Int]
                     , worryIncrease :: Int -> Int
                     , testMod :: Int
                     , throwToTrue :: Int
                     , throwToFalse :: Int
                     , itemsInspected :: Int}

makeMonkey :: [String] -> Monkey
makeMonkey s = Monkey { monkeyName = (read (take ((length (head s)) - 8) (drop 7 (head s))))::Int
                     , items = makeItemsList (drop 18 (head (tail s)))
                     , worryIncrease = makeWorryFunc (drop 23 (head (tail (tail s))))
                     , testMod = (read (drop 21 (head (tail (tail (tail s))))))::Int
                     , throwToTrue = (read (drop 28 (head (tail (tail (tail (tail s)))))))::Int
                     , throwToFalse = (read (drop 29 (head (tail (tail (tail (tail (tail s))))))))::Int
                     , itemsInspected = 0 }

makeWorryFunc :: String -> (Int -> Int)
makeWorryFunc (s:ss)
  | s == '*' = makeWorryFuncHelper (*) (tail ss)
  | s == '+' = makeWorryFuncHelper (+) (tail ss)

makeWorryFuncHelper :: (Int -> Int -> Int) -> String -> (Int -> Int)
makeWorryFuncHelper op ('o':_) = (\x -> op x x)
makeWorryFuncHelper op s = (\x -> (op) x ((read s)::Int))

makeItemsList :: String -> [Int]
makeItemsList [] = []
makeItemsList s = map (\x -> ((read x)::Int)) newS
  where newS = mySplit (s ++ ", ") ""

mySplit :: String -> String -> [String]
mySplit [] _ = []
mySplit (s:ss) c
  | s == ','  = c:(mySplit (tail ss) "")
  | otherwise = mySplit ss (c ++ [s])


throwTo :: [Monkey] -> Int -> Int -> [Monkey]
throwTo monkeys from to = let fromMonkey = monkeys!!from
                              toMonkey = monkeys!!to
                              newFromMonkey = Monkey { monkeyName = monkeyName fromMonkey
                                                     , items = tail (items fromMonkey)
                                                     , worryIncrease = worryIncrease fromMonkey
                                                     , testMod = testMod fromMonkey
                                                     , throwToTrue = throwToTrue fromMonkey
                                                     , throwToFalse = throwToFalse fromMonkey
                                                     , itemsInspected = itemsInspected fromMonkey }
                              newToMonkey = Monkey { monkeyName = monkeyName toMonkey
                                                   , items = (items toMonkey) ++ [((head (items fromMonkey)) `mod` (foldr (*) 1 (map testMod monkeys)))]
                                                   , worryIncrease = worryIncrease toMonkey
                                                   , testMod = testMod toMonkey
                                                   , throwToTrue = throwToTrue toMonkey
                                                   , throwToFalse = throwToFalse toMonkey
                                                   , itemsInspected = itemsInspected toMonkey }
                          in updateMonkey to newToMonkey (updateMonkey from newFromMonkey monkeys)

updateMonkey :: Int -> Monkey -> [Monkey] -> [Monkey]
updateMonkey 0 m ms = m:(tail ms)
updateMonkey n m ms = (head ms):(updateMonkey (n - 1) m (tail ms))
 


inspectItem :: Monkey -> Monkey
inspectItem m = Monkey { monkeyName = monkeyName m
                       , items = ((worryIncrease m) (head (items m))):(tail (items m))
                       , worryIncrease = worryIncrease m
                       , testMod = testMod m
                       , throwToTrue = throwToTrue m
                       , throwToFalse = throwToFalse m
                       , itemsInspected = (itemsInspected m) + 1 }

myDiv :: Int -> Int -> Int
myDiv a b = ((a - (a `mod` b)) `div` b)

monkeyTurn :: Int -> [Monkey] -> [Monkey]
monkeyTurn n m = let itemCount = length (items (m!!n))
                     in if (itemCount == 0)
                        then m
                        else let inspectedM = inspectItem (m!!n)
                                 to = if (head (items inspectedM) `mod` (testMod inspectedM)) == 0
                                   then throwToTrue inspectedM
                                   else throwToFalse inspectedM
                             in monkeyTurn n (throwTo (updateMonkey n inspectedM m) n to)

monkeyRound :: [Monkey] -> [Monkey]
monkeyRound m = monkeyRoundHelper 0 (length m) m

monkeyRoundHelper :: Int -> Int -> [Monkey] -> [Monkey]
monkeyRoundHelper n m monkeys
  | n < m = monkeyRoundHelper (n + 1) m (monkeyTurn n monkeys)
  | otherwise = monkeys

monkeyRounds :: Int -> [Monkey] -> [Monkey]
monkeyRounds 0 m = m
monkeyRounds n m = monkeyRounds (n - 1) (monkeyRound m)

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = insertSort x (sort xs)

insertSort :: Int -> [Int] -> [Int]
insertSort x [] = [x]
insertSort x (y:ys)
  | x > y = x:(y:ys)
  | otherwise = y:(insertSort x ys)

score :: [Int] -> Int
score (x:(y:ys)) = x * y

main :: IO ()
main = do
  [fileInput, rounds] <- getArgs
  fileContents <- readFile fileInput
  let monkeys = map makeMonkey (chunksOf 7 (lines fileContents))
    in putStrLn (show (score (sort (map itemsInspected (monkeyRounds ((read rounds)::Int) monkeys)))))
