module AoCLib.AoC2022
  (
    myReadInt,
    countLists,
    commonElement,
    splitListInHalf,
    findDuplicate,
    packRucksack,
    findCommon,
    findCommonElement,
    finduniqueSequence
  ) where

import qualified Data.Set as Set

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

commonElement :: [String] -> Char
commonElement ((x:xs):ys) =
  if (commonElementHelper x ys)
  then x
  else commonElement (xs:ys)

commonElementHelper :: Char -> [String] -> Bool
commonElementHelper _ [] = False
commonElementHelper c ([]:ys) = False
commonElementHelper c ((x:xs):[]) =
  if (c == x)
  then True
  else commonElementHelper c (xs:[])
commonElementHelper c ((x:xs):ys) =
  if (c == x)
  then commonElementHelper c ys
  else commonElementHelper c (xs:ys)

splitListInHalf :: String -> (String, String)
splitListInHalf ls = splitAt ((length ls) `div` 2) ls

findDuplicate :: (String, String) -> Char
findDuplicate ((x:xs), y) =
  if (elem x y)
     then x
     else (findDuplicate (xs, y))

packRucksack :: String -> (Set.Set Char, Set.Set Char)
packRucksack ls = let (compartment1, compartment2) = splitAt ((length ls) `div` 2) ls
                      in ((Set.fromList compartment1), (Set.fromList compartment2))

findCommon :: (Set.Set Char, Set.Set Char) -> Char
findCommon (compartment1, compartment2) = (head (Set.toList (Set.intersection compartment1 compartment2)))

findCommonElement :: [String] -> Char
findCommonElement (x:xs) = findCommonElementHelper (Set.fromList x) (map Set.fromList xs)

findCommonElementHelper :: Set.Set Char -> [Set.Set Char] -> Char
findCommonElementHelper c [] = head (Set.toList c)
findCommonElementHelper c (x:xs) = findCommonElementHelper (Set.intersection c x) xs

-- Note - for Day 6, this will return the START of the sequence (as a zero-indexed count), not the end (as a one-indexed count) as required, so add 4 to the results
findUniqueSequence :: Int -> String -> Int
findUniqueSequence lengthOfSequence searchString = findUniqueSequenceHelper 0 lengthOfSequence [] searchString

findUniqueSequenceHelper :: Int -> Int -> String -> String -> Int
findUniqueSequenceHelper startIndex sequenceLength [] (topOfSequence:restOfSequence) = findUniqueSequenceHelper (startIndex + 1) sequenceLength (topOfSequence:[]) restOfSequence 
findUniqueSequenceHelper startIndex sequenceLength (currentSequenceDrop:currentSequence) (nextChar:remainingSearchString)
  | (sequenceLength == ((length currentSequence) + 1)) && (not uniqueSequence) = findUniqueSequenceHelper (startIndex +1) sequenceLength (currentSequence ++ (list nextChar)) remainingSearchString
  | (sequenceLength == ((length currentSequence) + 1)) && uniqueSequence = startIndex
  | otherwise = findUniqueSequenceHelper (startIndex + 1) sequenceLength (currentSequence ++ (list nextChar)) remainingSearchString
  where uniqueSequence = isUniqueSequence (currentSequence ++ (list nextChar))
  
isUniqueSequence :: String -> Bool
isUniqueSequence [] = True
isUniqueSequence (x:xs) 
  | x `elem` xs = False
  | otherwise   = isUniqueSequence xs
  
  
