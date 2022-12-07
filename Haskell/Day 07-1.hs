-- Advent of Code 2022 - Day 7 Puzzle 1
-- Alex Lambert
-- aoc@penwing.me.uk

import Data.String
import Data.List.Split
import System.Environment
import AoCLib.AoC2022 as AoC

import Data.List.Split

data MyDirectory = MyFile { fileName::String 
                          , fileSize::Int }
                 | MyDirectory { dirName::String 
                               , dirSize::Int 
                               , dirContents::[MyDirectory]
                               , fileContents::[MyDirectory] } deriving (Show)

data Command = CD String
             | Down
             | Listing [MyDirectory] deriving (Show)
             

rootDir = MyDirectory {dirName="/", dirSize=0, dirContents=[], fileContents=[]}


addToDirectory :: MyDirectory -> MyDirectory -> MyDirectory
addToDirectory (MyDirectory newDirName newDirSize newDirContents newFileContents) (MyDirectory currentDirName currentDirSize currentDirContents currentFileContents) = (MyDirectory currentDirName currentDirSize ((MyDirectory newDirName newDirSize newDirContents newFileContents):currentDirContents) currentFileContents)
addToDirectory (MyFile newFileName newFileSize) (MyDirectory currentDirName currentDirSize currentDirContents currentFileContents) = (MyDirectory currentDirName currentDirSize currentDirContents ((MyFile newFileName newFileSize):currentFileContents))

getDirectory :: String -> MyDirectory -> MyDirectory
getDirectory dirName (MyDirectory _ _ contents _) = getDirectoryHelper dirName contents

getDirectoryHelper :: String -> [MyDirectory] -> MyDirectory
getDirectoryHelper dirName ((MyDirectory topDirName topDirSize topDirContents topFileContents):rest) 
  | dirName == topDirName = (MyDirectory topDirName topDirSize topDirContents topFileContents)
  | otherwise = getDirectoryHelper dirName rest
getDirectoryHelper dirName ((MyFile _ _):rest) = getDirectoryHelper dirName rest

updateSizes :: MyDirectory -> MyDirectory
updateSizes (MyFile n s) = (MyFile n s)
updateSizes (MyDirectory n s c f) = let updatedContents = map updateSizes c
  in (MyDirectory n ((foldr (+) 0 (map getSize updatedContents)) + (foldr (+) 0 (map getSize f))) updatedContents f)

getSize :: MyDirectory -> Int
getSize (MyFile _ s) = s
getSize (MyDirectory _ s _ _) = s

makeCommands :: [String] -> [Command]
makeCommands [] = []
makeCommands ((c:(" ":cs)):rest)
  | c == '$' && cs == "cd .." = (Down:(makeCommands rest))
  | c == '$' && (take 2 cs) == "cd" = ((CD (drop 3 cs)):(makeCommands rest))
  | c == '$' && cs == "ls" = ((Listing getListing rest):(makeCommands rest))
  | otherwise = makeCommands rest

getListing :: [String] -> [MyDirectory]
getListing [] = []
getListing (x:xs) 
  | (head x) == '$' = []
  | (splitX!!0) == "dir" = (MyDirectory (splitX!!1) 0 [] []):(getListing xs)
  | otherwise = (MyFile splitX!!1 (read (splitX!!0))::Int):(getListing xs)
    where splitX = splitOn " " x


main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  putStrLn (show (fileContents))
