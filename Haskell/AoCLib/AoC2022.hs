module AoCLib.AoC2022
  (
    myReadInt,
    countLists,
    commonElement,
    splitListInHalf,
    findDuplicate
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

