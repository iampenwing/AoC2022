-- Advent of Code 2022 - Day 7 Puzzle 1
-- Alex Lambert
-- aoc@penwing.me.uk

import Data.String
import Data.List.Split
import System.Environment
import AoCLib.AoC2022 as AoC

data MyDirectory = MyFile { fileName::String 
                          , fileSize::Int }
                 | MyDirectory { dirName::String 
                               , dirSize::Int 
                               , dirContents::[MyDirectory] } deriving (Show)

testData = MyDirectory {dirName="/", dirSize=0, dirContents=[(MyFile {fileName="a.txt", fileSize=200}), (MyDirectory {dirName="dirA", dirSize=0, dirContents=[(MyFile {fileName="b.txt", fileSize=200}),(MyFile {fileName="c.txt", fileSize=200})]})]}

updateSizes :: MyDirectory -> MyDirectory
updateSizes (MyFile n s) = (MyFile n s)
updateSizes (MyDirectory n s c) = let updatedContents = map updateSizes c
  in (MyDirectory n (foldr (+) 0 (map getSize updatedContents)) updatedContents)

getSize :: MyDirectory -> Int
getSize (MyFile _ s) = s
getSize (MyDirectory _ s _) = s

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  putStrLn (show (fileContents))
