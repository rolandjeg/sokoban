module Sokoban where

import Data.Char
import Data.List
import Parser

type Coord = (Int, Int)

data World = World { walls :: [Coord]
                   , crates :: [Coord]
                   , storage :: [Coord]
                   , empty :: [Coord]
                   , startWorker :: Coord
                   , startCrates :: [Coord]
                   , worker :: Coord
                   , steps :: Int
                   , pushes :: Int
                   , name :: String
                   , path :: [Action]
                   } deriving (Show)

emptyWorld :: World
emptyWorld = World { walls = []
                   , crates = []
                   , startCrates = []
                   , storage = []
                   , empty = []
                   , worker = (-1,1)
                   , startWorker = (-1,1)
                   , name = ""
                   , path = []
                   , steps = 0 
                   , pushes = 0 }

data Action = SUp 
            | SDown 
            | SLeft 
            | SRight
            | SUndo
           deriving (Show, Eq, Ord)

walking :: World -> [Coord]
walking world = walking' [worker world] [worker world]
    where
        walking' :: [Coord] -> [Coord] ->  [Coord]
        walking' [] searched = searched
        walking' ((x,y):xs) searched = 
            walking' tocheck searched'
                where 
                    checkUp = [(x,y-1) | (x,y-1) `notElem` walls world, (x,y-1) `notElem` searched] 
                    checkDown = [(x,y+1) | (x,y+1) `notElem` walls world, (x,y+1) `notElem` searched] 
                    checkLeft = [(x-1,y) | (x-1,y) `notElem` walls world, (x-1,y) `notElem` searched] 
                    checkRight = [(x+1,y) | (x+1,y) `notElem` walls world, (x+1,y) `notElem` searched] 
                    searched' = checkUp ++ checkDown ++ checkLeft ++ checkRight ++ searched
                    tocheck = checkUp ++ checkDown ++ checkLeft ++ checkRight ++ xs

emptyTiles :: World -> [Coord]
emptyTiles world = 
    [x | x <- reachableTiles world , x `notElem` (walls world ++ storage world)]
        where reachableTiles = walking

applyInput :: Coord -> Action-> Coord
applyInput (x,y) SUp    = (x   , y-1)
applyInput (x,y) SLeft  = (x-1 , y)
applyInput (x,y) SRight = (x+1 , y)
applyInput (x,y) SDown  = (x   , y+1) 
-- applyInput (x,y) _ = (x,y)

isWall :: World -> Coord -> Bool
isWall world coord = coord `elem` walls world

isCrate :: World -> Coord -> Bool
isCrate world coord = coord `elem` crates world

isValid :: World -> Action -> Bool
isValid world input
        | input == SUndo && steps world > 0 = True
        | input == SUndo && steps world <= 0 = False
        | isWall world newpos = False
        | isCrate world newpos = not (isWall world newpos' || isCrate world newpos')
        | otherwise = True
            where oldpos = worker world
                  newpos = applyInput oldpos input
                  newpos' = applyInput newpos input

followPath world path =
    let startWorld = world { crates = startCrates world
                           , path = []
                           , steps = 0
                           , pushes = 0
                           , worker = startWorker world }
        followPath' = foldl modifyWorld
    in followPath' startWorld (reverse path)

modifyWorld :: World -> Action -> World
modifyWorld world input = if isValid world input then go world input else world
    where 
        go world input =
            let inputcoord = applyInput (worker world) input
                inputcoord' = applyInput inputcoord input
                -- lastmove = head (path world)
             in
             case input of
               SUndo -> followPath world (tail (path world))
               _     ->
                       if inputcoord `elem` crates world
                          then world { crates = inputcoord':filter (/= inputcoord) (crates world)
                                     , worker = inputcoord
                                     , path = input:path world
                                     , steps = steps world + 1 
                                     , pushes = pushes world + 1 }
                          else world { worker = inputcoord
                                     , path = input:path world
                                     , steps = steps world + 1 }


getMaxWidth :: World -> Int
getMaxWidth world = maximum [ foldl (\m (x,_) -> max m x) 0 (crates world)
                            , foldl (\m (x,_) -> max m x) 0 (storage world)
                            , foldl (\m (x,_) -> max m x) 0 (walls world) ]

getMaxHeight :: World -> Int
getMaxHeight world = maximum [ foldl (\m (_,y) -> max m y) 0 (crates world)
                            , foldl (\m (_,y) -> max m y) 0 (storage world)
                            , foldl (\m (_,y) -> max m y) 0 (walls world) ]

isFinished :: World -> Bool
isFinished world = isFinished' (crates world) world

isFinished' :: [Coord] -> World -> Bool
isFinished' [] _ = True
isFinished' (x:xs) world = x `elem` storage world && isFinished' xs world

getScore :: World -> String
getScore world = "Moves: " ++ show (steps world) ++ " Pushes: " ++ show (pushes world)

addEmpty :: World -> World
addEmpty world =
    world { empty = emptyTiles world}

buildLevel :: String -> World
buildLevel x = addEmpty $ buildLevel' x emptyWorld 0 0

buildLevel' :: String -> World -> Int -> Int -> World
buildLevel' "" world _ _ = world
buildLevel' (s:ss) world x y = 
    case s of
      ' ' -> buildLevel' ss world (x+1) y
      '#' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = crates world 
                             , walls = (x,y):walls world
                             , storage = storage world
                             , worker = worker world
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      'O' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = (x,y):crates world 
                             , startCrates = (x,y):startCrates world 
                             , walls = walls world
                             , storage = storage world
                             , worker = worker world
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      '$' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = (x,y):crates world 
                             , startCrates = (x,y):startCrates world 
                             , walls = walls world
                             , storage = storage world
                             , worker = worker world
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      '.' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = crates world 
                             , walls = walls world
                             , storage = (x,y):storage world
                             , worker = worker world
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      '*' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = (x,y):crates world 
                             , startCrates = (x,y):startCrates world 
                             , walls = walls world
                             , storage = (x,y):storage world
                             , worker = worker world
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      '0' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = (x,y):crates world 
                             , startCrates = (x,y):startCrates world 
                             , walls = walls world
                             , storage = (x,y):storage world
                             , worker = worker world
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      '@' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = crates world 
                             , walls = walls world
                             , storage = storage world
                             , worker = (x,y)
                             , startWorker = (x,y)
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      'A' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = crates world 
                             , walls = walls world
                             , storage = (x,y):storage world
                             , worker = (x,y)
                             , startWorker = (x,y)
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      '+' -> buildLevel' ss world' (x+1) y
        where world' = world { crates = crates world 
                             , walls = walls world
                             , storage = (x,y):storage world
                             , worker = (x,y)
                             , startWorker = (x,y)
                             , name = name world
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      '\n' -> buildLevel' ss world 0 (y+1)
      ';' -> world'
        where world' = world { crates = crates world 
                             , walls = walls world
                             , storage = storage world
                             , worker = worker world
                             , name = ss
                             , pushes = pushes world
                             , empty = empty world
                             , steps = steps world }
      _ -> error ("Wrong World " ++ show s)


getFirst :: String -> String
getFirst contents 
    | ';' `elem` contents =
        unlines (takeWhile sp contentlines ++ [head (dropWhile sp contentlines)])
    | otherwise = ""
        where sp x = head x /= ';'
              contentlines = lines contents

getSecond :: String -> String
getSecond contents 
    | ';' `elem` contents =
        unlines (tail (dropWhile sp contentlines))
    | otherwise = ""
        where sp x = head x /= ';'
              contentlines = lines contents

splitContents' :: String -> [String] -> [String]
splitContents' contents strings 
    | getFirst contents /= "" =
        getFirst contents : splitContents' (getSecond contents) strings
    | otherwise = strings

splitContents :: String -> [String]
splitContents contents =
    splitContents' contents []

loadLevel :: String -> IO [World] 
loadLevel filename = do
    contents <- readFile filename
    let contents2 = (unlines . filter (/= "") . lines) contents
    let contentlist = splitContents contents2

    return (map buildLevel contentlist)
