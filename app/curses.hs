import Sokoban
import Parser

import System.Environment
import Data.Char
import Data.List
import Control.Monad
import Control.Exception

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
--import Data.Maybe

data Input = IUp 
           | IDown 
           | ILeft 
           | IRight
           | IQuit
           | IUndo
           | IRestart
           deriving (Show, Eq, Ord)

data Keybindings = Keybindings { cLeft :: Char
                   , cRight :: Char
                   , cDown :: Char
                   , cUp :: Char
                   , cQuit :: Char
                   , cUndo :: Char
                   , cRestart :: Char
                   }

bindings :: Keybindings
bindings = Keybindings { cLeft = 'h'
                       , cRight = 'l'
                       , cDown = 'j'
                       , cUp = 'k'
                       , cQuit = 'q'
                       , cUndo = 'u'
                       , cRestart = 'r'
                       }

getInput :: IO Input
getInput = do
    c <- getChar
    -- c <- getKey
    go c
        where
            go c
                | c == cLeft bindings = return ILeft
                | c == cRight bindings = return IRight
                | c == cDown bindings = return IDown
                | c == cUp bindings = return IUp
                | c == cQuit bindings = return IQuit
                | c == cUndo bindings = return IUndo
                | c == cRestart bindings = return IRestart
                | otherwise = getInput
        
    --case c of
      --(cLeft bindings) -> return ILeft
      --(cright bindings) -> return IRight
      --(cDown bindings) -> return IDown
      --(cUp bindings) -> return IUp
      --(cQuit bindings) -> return IQuit
      --(cUndo bindings) -> return IUndo
      --(cRestart bindings) -> return IRestart
      --_   -> getInput

offset :: Int
offset = 3

displayWalls :: Coord -> IO ()
displayWalls (x,y) = 
    mvAddCh (y+offset) x c
        where c = fromIntegral (ord '#')

displayStorage :: Coord -> IO ()
displayStorage (x,y) = do
    attrSet attr0 (Pair 4)
    mvAddCh (y+offset) x c
    attrSet attr0 (Pair 1)
        where c = fromIntegral (ord '.')

displayEmpty :: Coord -> IO ()
displayEmpty (x,y) = do
    attrSet attr0 (Pair 1)
    mvAddCh (y+offset) x c
    attrSet attr0 (Pair 1)
        where c = fromIntegral (ord '.')

displayCrates :: Coord -> IO ()
displayCrates (x,y) = 
    mvAddCh (y+offset) x c
        where c = fromIntegral (ord 'O')

displayCratesOnStorage :: Coord -> IO ()
displayCratesOnStorage (x,y) = do
    attrSet attr0 (Pair 2)
    mvAddCh (y+offset) x c
    attrSet attr0 (Pair 1)
        where c = fromIntegral (ord '0')

displayWorker :: Coord -> IO ()
displayWorker (x,y) =  
    mvAddCh (y+offset) x c
        where c = fromIntegral (ord '@')

displayWorkerOnStorage :: Coord -> IO ()
displayWorkerOnStorage (x,y) =  do
    attrSet attr0 (Pair 3)
    mvAddCh (y+offset) x c
    attrSet attr0 (Pair 1)
        where c = fromIntegral (ord '@')

-- intersect :: (Eq a) => [a] -> [a] -> [a]
-- intersect [] _ = []
-- intersect (x:xs) y = if x `elem` y
                        -- then x:intersect xs y
                        -- else intersect xs y

displayWorld :: World -> IO ()
displayWorld world = do
    wclear stdScr
    mvWAddStr stdScr 0 0 stepstr 
    mvWAddStr stdScr 1 0 namestr 

    mapM_ displayEmpty (empty world)
    mapM_ displayWalls (walls world)
    mapM_ displayStorage (storage world)
    mapM_ displayCrates (crates world)
    mapM_ displayCratesOnStorage (crates world `intersect` storage world)

    if worker world `elem` storage world
       then displayWorkerOnStorage (worker world)
       else displayWorker (worker world)
    refresh
        where 
              stepstr = "Moves: " ++ show (steps world) ++ " Pushes: " ++ show (pushes world)
              namestr = "Level: " ++ name world

isAction :: Input -> Bool
isAction input = input `elem` [IUp, IDown, ILeft, IRight, IUndo]

getAction :: Input -> Action
getAction IUp = SUp
getAction IDown = SDown
getAction ILeft = SLeft
getAction IRight = SRight
getAction IUndo = SUndo
getAction _ = error "No Action"

winscreen :: World -> IO ()
winscreen world = do
    wclear stdScr
    mvWAddStr stdScr 0 0 stepstr 
    mvWAddStr stdScr 1 0 namestr 

    mapM_ displayEmpty (empty world)
    mapM_ displayWalls (walls world)
    mapM_ displayStorage (storage world)
    mapM_ displayCrates (crates world)
    mapM_ displayCratesOnStorage (intersect (crates world) (storage world))

    if worker world `elem` storage world
       then displayWorkerOnStorage (worker world)
       else displayWorker (worker world)
    move y x
    let pos = getMaxHeight world + offset + 2
    mvWAddStr stdScr pos 0 winstr
    refresh
    _ <- getChar
    menu
        where y = snd $ worker world
              x = fst $ worker world
              stepstr = "Moves: " ++ show (steps world) ++ " Pushes: " ++ show (pushes world)
              namestr = "Level: " ++ name world
              winstr = "Congratulations! Press any Key to return to the menu."


gameloop :: World -> IO ()
gameloop world = do
    displayWorld world
    input <- getInput
    case input of
      IQuit -> do
          q <- quitDialog "Return to menu?"
          if q 
             then menu
             else gameloop world 
      _ -> do
          let world' = 
                  if isAction input 
                      then modifyWorld world (getAction input)
                      else world
          if isFinished world' 
             then winscreen world'
             else gameloop world'

quitDialog :: String -> IO Bool
quitDialog msg = do
    wclear stdScr
    mvWAddStr stdScr 0 0 (msg ++ " (Y/N)")
    refresh
    c <- getChar
    case c of
      'y' -> return True
      'n' -> return False
      _ -> quitDialog msg
        
getChooseInput :: Int -> Int -> Int -> [World] -> IO Int
getChooseInput y m page worlds = do
    wclear stdScr
    displayWorldList' pageWorlds 0
    move y 0 
    refresh
    c <- getCh
    case c of
      KeyChar 'j' -> getChooseInput (min (y + 1) (length pageWorlds - 1)) m page worlds
      KeyDown -> getChooseInput (min (y + 1) (maxHeight - 1)) m page worlds
      KeyChar 'k' -> getChooseInput (max (y-1) 0) m page worlds
      KeyUp -> getChooseInput (max (y-1) 0) m page worlds
      KeyChar 'h' -> prevPage
      KeyLeft -> prevPage
      KeyChar 'l' -> nextPage
      KeyRight -> nextPage
      KeyChar ' ' -> return (y+page*23)
      KeyChar '\r' -> return (y+page*23)
      KeyChar 'q' -> do 
          q <- quitDialog "Really Quit?"
          if q 
             then return (-1)
             else return (-2) 
      _   -> getChooseInput y m page worlds
    where 
        pageWorlds = take 23 (drop (page * 23) worlds)
        displayWorldList' [] _ = return ()
        displayWorldList' (x:xs) i 
            | i < maxHeight - 1 = do 
                mvWAddStr stdScr i 0 ((show (i+(page*23))) ++ ": " ++ name x)
                displayWorldList' xs (i+1)
            | i == maxHeight - 1 = 
                mvWAddStr stdScr i 0 moreStr
            | otherwise = return ()
        maxHeight = 24
        moreStr = " -----> " ++ show page
        nextPage =
            if m > (page + 1) * 23 
               then getChooseInput y m (page + 1) worlds
               else getChooseInput y m page worlds
        prevPage =
            if page > 0 
               then getChooseInput y m (page - 1) worlds
               else getChooseInput y m page worlds

menu :: IO ()
menu = do
    wclear stdScr
    --filename <- getArgs
    args <- getArgs
    let filename = getFilename args
    case filename of
      Nothing -> noFileError
      Just fname -> do
       result <- try (loadLevel fname) :: IO (Either SomeException [World])
       case result of
         Left ex -> wrongFileError ex
         Right worlds -> do 
             --displayWorldList worlds
             refresh
             _ <- cursSet CursorVisible
             input <- getChooseInput 0 (length worlds - 1) 0 worlds
             _ <- cursSet CursorInvisible
             case input of
               -1 -> return ()
               -2 -> menu
               _ ->  void (gameloop (worlds !! input))
    where
        getFilename :: [String] -> Maybe String
        getFilename [] = Nothing 
        getFilename (x:_) = Just x

        noFileError = end >> putStrLn "No filename specified"
        wrongFileError ex = end  >> print ex

main :: IO ()
main = do
    -- init
    start
    startColor
    initPair (Pair 1) white black -- normal
    initPair (Pair 2) white blue -- crates on storage
    initPair (Pair 3) white blue -- worker on storage
    initPair (Pair 4) white blue -- storage
    _ <- cursSet CursorInvisible
    menu 
    end
