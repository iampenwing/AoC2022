module AoCLib.AoC2022
  (
    myReadInt,
    countLists
  ) where

myReadInt :: [Char] -> Int
myReadInt ('+':xs) = read xs ::Int
myReadInt x = read x :: Int

countLists :: [String] -> [Int]
countLists [] = []
countLists (x:xs) =
  let (topCount, rest) = (iCountLists 0 (x:xs))
  in (topCount:(countLists rest))

iCountLists :: Int -> [String] -> (Int, [String])
iCountLists count [] = (count, [])
iCountLists count ("":xs) = (count, xs)
iCountLists count (x:xs) = iCountLists (count + (myReadInt x)) xs

