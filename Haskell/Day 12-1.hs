data EdgeSpec = EdgeSpec { fromNode :: (Int, Int)
                         , toNode :: (Int, Int)
                         , distance :: Int } deriving (Show)

--makeEdges :: (Int, Int) -> [String] -> ([Edge], (Int, Int), (Int, Int))
--makeEdges x y s = let surrounds = getSurroundingNodes s (x, y)
                    -- in ()

makeEdges :: [String] -> [EdgeSpec]
makeEdges s = makeEdgesHelper (length (head s)) (length s) (0, 0) s

makeEdgesHelper :: Int -> Int -> (Int,Int) -> [String]-> [EdgeSpec]
makeEdgesHelper maxX maxY (x, y) s 
  | x == maxX && y == (maxY - 1) = []
  | x == maxX = makeEdgesHelper maxX maxY (0, (y + 1)) s
  | otherwise = (makeEdgesFromNodes (x, y) (getSurroundingNodes s (x, y))) ++ (makeEdgesHelper maxX maxY ((x + 1), y) s)

makeEdgesFromNodes :: (Int, Int) -> [(Int, Int)] -> [EdgeSpec]
makeEdgesFromNodes _ [] = []
makeEdgesFromNodes (fX, fY) (x:xs) = (EdgeSpec { fromNode = (fX, fY), toNode = x, distance = 1}):(makeEdgesFromNodes (fX, fY) xs)

getSurroundingNodes :: [String] -> (Int, Int) -> [(Int, Int)]
getSurroundingNodes s (x, y) 
  | x == 0 && y == 0 = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y+1)),((x+1), y)] -- Top Left
  | x == ((length (head s)) - 1) && y == ((length s) - 1) = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y-1)),((x-1), y)] -- Bottom Right
  | x == ((length (head s)) - 1) && y == 0 = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [((x - 1), y),(x, (y + 1))] -- Top Right
  | x == 0 && y == ((length s) - 1) = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y-1)),((x+1), y)] -- Bottom Left
 | x == 0 = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y+1)),((x+1), y), ((x+1), (y-1))] -- Left Edge
  | y == 0 = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y+1)),((x+1), y),((x-1),y)] -- Top Edge
  | x == ((length (head s)) - 1) = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y+1)),((x-1), y), (x, (y-1))] -- Right Edge
  | y == ((length s) - 1) = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y-1)),((x-1), y), ((x+1),(y-1))] -- Bottom Edge
  | otherwise = filter (\(nx, ny) -> (makeHeight ((s!!y)!!x)) >= ((makeHeight ((s!!ny)!!nx)) - 1)) [(x, (y-1)),((x-1), y), ((x),(y+1)), ((x+1),y)] -- Middle

makeHeight :: Char -> Int
makeHeight c
  | c == 'a' = 0
  | c == 'b' = 1
  | c == 'c' = 2
  | c == 'd' = 3
  | c == 'e' = 4
  | c == 'f' = 5
  | c == 'g' = 6
  | c == 'h' = 7
  | c == 'i' = 8
  | c == 'j' = 9
  | c == 'k' = 10
  | c == 'l' = 11
  | c == 'm' = 12
  | c == 'n' = 13
  | c == 'o' = 14
  | c == 'p' = 15
  | c == 'q' = 16
  | c == 'r' = 17
  | c == 's' = 18
  | c == 't' = 19
  | c == 'u' = 20
  | c == 'v' = 21
  | c == 'w' = 22
  | c == 'x' = 23
  | c == 'y' = 24
  | c == 'z' = 25
  | c == 'S' = 0
  | c == 'E' = 25

input = ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]

-- heights = map (\x -> map makeHeights x) input



main :: IO()
main = putStrLn (show (makeEdges input))
