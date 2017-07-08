{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import SDL 

import Bindings

import Reactive.Banana
import Reactive.Banana.SDL2
import Reactive.Banana.Frameworks as RBF
import Reactive.Banana.Combinators as RBC

--import qualified Control.Exception as CE

import qualified SDL.Font as Font
import qualified SDL.Image as Image
import Linear (V4(..))
import Control.Monad (unless, when, liftM)
import Control.Exception
import System.Environment
import Foreign.C.Types 
import System.Exit (exitSuccess)
import System.Directory
import System.FilePath
import qualified Data.Text as Text
import Data.List

import Sokoban

height,width :: CInt
width = 800
height = 600
spriteSize = 20
statusLineHeight = 50


data State = Menu | Ingame | Worlds | Finished
  deriving (Show, Ord, Eq)

data Tiles = Tiles { work :: Texture
                   , workDocked :: Texture
                   , empt :: Texture
                   , wall :: Texture
                   , box :: Texture
                   , boxDocked :: Texture
                   , dock :: Texture }

changeState :: State -> State
changeState Menu = Ingame
changeState Ingame = Menu
changeState Worlds = Ingame
changeState Finished = Menu

acc :: Int -> Int -> Int
acc m x = min m (x+1)

dec :: Int -> Int
dec x = max 0 (x-1)

data Control = Control { eQuit :: RBC.Event Keysym 
                       , eMenu :: RBC.Event Keysym
                       , ePause :: RBC.Event Keysym
                       , eLeft :: RBC.Event Keysym
                       , eRight :: RBC.Event Keysym
                       , eUp :: RBC.Event Keysym
                       , eDown :: RBC.Event Keysym
                       , eUndo :: RBC.Event Keysym
                       , eEnter :: RBC.Event Keysym }

controls :: SDLEventSource -> MomentIO Control
controls eventsource = mdo
  esdl <- sdlEvent eventsource
  return Control { eQuit = filterE (\a -> keysymKeycode a == bindings "q" ) (keyDownEvent esdl)
                 , eMenu = filterE (\a -> keysymKeycode a == SDL.KeycodeM ) (keyDownEvent esdl)
                 , ePause = filterE (\a -> keysymKeycode a == SDL.KeycodeP ) (keyDownEvent esdl)
                 , eLeft = filterE (\a -> keysymKeycode a == SDL.KeycodeH ) (keyDownEvent esdl)
                 , eRight = filterE (\a -> keysymKeycode a == SDL.KeycodeL ) (keyDownEvent esdl)
                 , eUp = filterE (\a -> keysymKeycode a == SDL.KeycodeK ) (keyDownEvent esdl)
                 , eDown = filterE (\a -> keysymKeycode a == SDL.KeycodeJ ) (keyDownEvent esdl)
                 , eUndo = filterE (\a -> keysymKeycode a == SDL.KeycodeU ) (keyDownEvent esdl)
                 , eEnter = filterE (\a -> keysymKeycode a == SDL.KeycodeReturn ) (keyDownEvent esdl) }

networkDescription :: SDLEventSource -> Renderer -> Tiles -> Font.Font -> MomentIO ()
networkDescription eventsource renderer tiles font = mdo
  etick <- tickEvent eventsource
  esdl <- sdlEvent eventsource

  let 
      movedown :: Point V2 CInt -> Point V2 CInt
      movedown (P(V2 a b)) = P(V2 a (b+20))
      moveup :: Point V2 CInt -> Point V2 CInt
      moveup (P(V2 a b)) = P(V2 a (b-20))
      moveleft :: Point V2 CInt -> Point V2 CInt
      moveleft (P(V2 a b)) = P(V2 (a-20) b)
      moveright :: Point V2 CInt -> Point V2 CInt
      moveright (P(V2 a b)) = P(V2 (a+20) b)
      startpos = P(V2 0 0) :: Point V2 CInt

  control <- controls eventsource
  
  bState <- accumB Menu $ unions [ changeState <$ eMenu control
                                 , const Worlds <$ whenE bMenu (eEnter control)
                                 , const Ingame <$ whenE bWorlds (eEnter control)
                                 , const Finished <$ whenE bGame eFinished 
                                 , const Worlds <$ whenE bFinished (eEnter control)
                                 ]
  let bMenu = (== Menu) <$> bState
  let bGame = (== Ingame) <$> bState
  let bWorlds = (== Worlds) <$> bState
  let bFinished = (== Finished) <$> bState

  -- bFinished <- accumB False $ 
  --bFinished <- newBehavior False

  (bMaxPos,eMaxPos) <- newBehavior 0


  bPos <- accumB startpos $ unions [ movedown <$ whenE bGame (eDown control)
                                   , moveup <$ whenE bGame (eUp control)
                                   , moveleft <$ whenE bGame (eLeft control)
                                   , moveright <$ whenE bGame (eRight control) ]

  bMenuPos <- accumB 0 $ unions [ acc <$> bMaxPos <@ whenE bMenu (eDown control)
                                , acc <$> bMaxPos <@ whenE bWorlds (eDown control)
                                , dec <$ whenE bMenu (eUp control)
                                , dec <$ whenE bWorlds (eUp control)
                                , const 0 <$ whenE bMenu (eEnter control)
                                , const 0 <$ whenE bGame (eMenu control)
                                ]

  (bLevelPath, eLevelPath) <- newBehavior (Text.pack "")
  (bWorldList, eWorldList) <- newBehavior []
  (eLoadWorld :: Reactive.Banana.Event (World -> World), fireLoadWorld :: RBF.Handler (World -> World) ) <- newEvent
  (eFinished, fireFinished) <- newEvent

  bWorld <- accumB emptyWorld $ unions [ eLoadWorld 
                                       , flip modifyWorld SLeft <$ whenE bGame (eLeft control)
                                       , flip modifyWorld SRight <$ whenE bGame (eRight control)
                                       , flip modifyWorld SUp <$ whenE bGame (eUp control)
                                       , flip modifyWorld SDown <$ whenE bGame (eDown control)
                                       , flip modifyWorld SUndo <$ whenE bGame (eUndo control)  ]

  let bTest = (,) <$> bWorldList <*> bMenuPos
      bLoadWorldInput = (,) <$> bWorldList <*> bMenuPos


  let eMenuTick = whenE bMenu etick
  let eGameTick = whenE bGame etick
  let eWorldsTick = whenE bWorlds etick
  let eFinishedTick = whenE bFinished etick

  reactimate $ loadWorld fireLoadWorld <$> bLoadWorldInput <@ whenE bWorlds (eEnter control)
  reactimate $ checkFinished fireFinished <$> bWorld <@ eGameTick
  reactimate $ loadWorldList eWorldList <$> bLevelPath <@ whenE bMenu (eEnter control)
  reactimate $ drawMenu renderer eMaxPos eLevelPath <$> bMenuPos <@ eMenuTick
  reactimate $ (quit >> exitSuccess) <$ eQuit control
  reactimate $ drawWorlds renderer eMaxPos <$> bTest <@ eWorldsTick
  reactimate $ drawGame renderer tiles font <$> bWorld <@ eGameTick
  reactimate $ drawFinished renderer <$ eFinishedTick
  reactimate $ present renderer <$ etick

drawFinished :: Renderer -> IO ()
drawFinished = clear

drawGame :: Renderer -> Tiles -> Font.Font -> World -> IO ()
drawGame renderer tiles font world =  do
  clear renderer
  drawStatusLine world font
  mapM_ (drawEmpty tiles) (Sokoban.empty world)
  mapM_ (drawWalls tiles) (Sokoban.walls world)
  mapM_ (drawStorage tiles) (storage world)
  mapM_ (drawCrates tiles) (crates world)
  mapM_ (drawCratesOnStorage tiles) (crates world `intersect` storage world)

  if Sokoban.worker world `elem` storage world
     then drawWorkerOnStorage tiles (Sokoban.worker world)
     else drawWorker tiles (Sokoban.worker world)
  where 
    drawEmpty :: Tiles -> Coord -> IO ()
    drawEmpty tiles coord = SDL.copy renderer (empt tiles) Nothing (convCoord coord)
    drawWalls tiles coord = SDL.copy renderer (wall tiles) Nothing (convCoord coord)
    drawStorage tiles coord = SDL.copy renderer (dock tiles) Nothing (convCoord coord)
    drawCrates tiles coord = SDL.copy renderer (box tiles) Nothing (convCoord coord)
    drawCratesOnStorage tiles coord = SDL.copy renderer (boxDocked tiles) Nothing (convCoord coord)
    drawWorkerOnStorage tiles coord = SDL.copy renderer (workDocked tiles) Nothing (convCoord coord)
    drawWorker tiles coord = SDL.copy renderer (work tiles) Nothing (convCoord coord)
    convCoord (x,y) = Just (Rectangle (P (V2 (toEnum (spriteSize*x)) (toEnum (spriteSize*y + statusLineHeight)))) pos2)
    drawStatusLine world font = do
      let stepstr = Text.pack $ "Moves: " ++ show (steps world) ++ " Pushes: " ++ show (pushes world)
      surf <- Font.solid font (V4 128 128 128 255) stepstr
      text <- createTextureFromSurface renderer surf
      tInfo <- queryTexture text
      let w = textureWidth tInfo
      let h = textureHeight tInfo
      SDL.copy renderer text Nothing (Just (Rectangle (P (V2 0 0)) (V2 w h)))
      SDL.freeSurface surf
      SDL.destroyTexture text
      --levelPathHandler $ baseDir `Text.append` x `Text.append` fileExt

loadWorld :: RBF.Handler (World -> World) -> ([World], Int) -> IO ()
loadWorld fireLoadWorld (worlds,pos) = fireLoadWorld (const (worlds !! pos))

checkFinished :: RBF.Handler () -> World -> IO ()
checkFinished fireFinished world = if isFinished world
                                      then fireFinished ()
                                      else return ()

main :: IO ()
main = do
  --worlds <- getWorlds
  --(sokoAddHandler,sokoHandler) <- newAddHandler
  let fps = 35
  let myWindow = defaultWindow { windowInitialSize = V2 width height }
  let myRenderer = defaultRenderer { rendererType = AcceleratedVSyncRenderer }
  initializeAll
  Font.initialize
  font <- Font.load "/usr/share/fonts/TTF/DejaVuSans.ttf" fontSize
  window <- createWindow "Sokoban" myWindow
  renderer <- createRenderer window (-1) defaultRenderer
  eventsource <- getSDLEventSource

  textures <- loadTextures renderer
  --hello <- Image.load "/home/ross/coding/sokoban/data/tiles/worker-docked.png"
  --hellotexture <- createTextureFromSurface renderer hello

  network <- compile $ networkDescription eventsource renderer textures font
  actuate network
  -- runCappedSDLPump fps eventsource
  runCappedSDLPump fps eventsource
    where
      loadTextures renderer = do
        workDocked <- Image.load "/home/ross/coding/sokoban/data/tiles/worker-docked.png"
        work <- Image.load "/home/ross/coding/sokoban/data/tiles/worker.png"
        empt <- Image.load "/home/ross/coding/sokoban/data/tiles/floor.png"
        wall <- Image.load "/home/ross/coding/sokoban/data/tiles/wall.png"
        box <- Image.load "/home/ross/coding/sokoban/data/tiles/box.png"
        boxDocked <- Image.load "/home/ross/coding/sokoban/data/tiles/box-docked.png"
        dock <- Image.load "/home/ross/coding/sokoban/data/tiles/dock.png"

        textWorkDocked <- createTextureFromSurface renderer workDocked
        textWork <- createTextureFromSurface renderer work
        textEmpt <- createTextureFromSurface renderer empt
        textWall <- createTextureFromSurface renderer wall
        textBox <- createTextureFromSurface renderer box
        textBoxDocked <- createTextureFromSurface renderer boxDocked
        textDock <- createTextureFromSurface renderer dock
        return $ Tiles { work = textWork
                       , workDocked = textWorkDocked
                       , empt = textEmpt
                       , wall = textWall
                       , box = textBox
                       , boxDocked = textBoxDocked
                       , dock = textDock } 


pos :: Point V2 CInt
pos = P (V2 20 20)
pos2 :: V2 CInt
pos2 = V2 20 20

loadWorldList :: RBF.Handler [World] -> Text.Text -> IO ()
loadWorldList fireWorldList filename = do
  result <- try (loadLevel (Text.unpack filename)) :: IO (Either SomeException [World])
  case result of
    Left ex -> wrongFileError ex
    Right worlds -> fireWorldList worlds
  where wrongFileError ex = print ex

drawWorlds :: Renderer -> RBF.Handler Int -> ([World], Int) -> IO ()
drawWorlds renderer maxPosHandler (worlds, position) = do
  clear renderer
  maxPosHandler $ length worlds - 1
  font <- Font.load "/usr/share/fonts/TTF/DejaVuSans.ttf" fontSize
  go font (fmap (Text.pack . name) worlds) 0 (toEnum position)
    where
      go _ [] _ _ = pure ()
      go font (x:xs) p 0 = do
        surf <- Font.solid font (V4 128 128 128 255) x
        text <- createTextureFromSurface renderer surf
        tInfo <- queryTexture text
        let w = textureWidth tInfo
        let h = textureHeight tInfo
        SDL.copy renderer text Nothing (Just (Rectangle (P (V2 0 p)) (V2 w h)))
        go font xs (p+h) (-1)
      go font (x:xs) p i = do
        surf <- Font.solid font (V4 255 255 255 255) x
        text <- createTextureFromSurface renderer surf
        tInfo <- queryTexture text
        let w = textureWidth tInfo
        let h = textureHeight tInfo
        SDL.copy renderer text Nothing (Just (Rectangle (P (V2 0 p)) (V2 w h)))
        go font xs (p+h) (i-1)

drawMenu :: Renderer -> RBF.Handler Int -> RBF.Handler Text.Text -> Int -> IO ()
drawMenu renderer maxPosHandler levelPathHandler position = do
  clear renderer
  filesTemp <- listDirectory "/home/ross/coding/sokoban/data/levels/"
  let files = map (Text.pack . takeBaseName ) filesTemp
  maxPosHandler $ length files - 1
  font <- Font.load "/usr/share/fonts/TTF/DejaVuSans.ttf" fontSize
  drawFiles font files 0 (toEnum position)
    where
      baseDir = Text.pack "/home/ross/coding/sokoban/data/levels/"
      fileExt = Text.pack ".txt"
      drawFiles :: Font.Font -> [Text.Text] -> CInt -> CInt -> IO ()
      drawFiles _ [] _ _ = pure ()
      drawFiles font (x:xs) p 0 = do
        surf <- Font.solid font (V4 128 128 128 255) x
        text <- createTextureFromSurface renderer surf
        tInfo <- queryTexture text
        let w = textureWidth tInfo
        let h = textureHeight tInfo
        SDL.copy renderer text Nothing (Just (Rectangle (P (V2 0 p)) (V2 w h)))
        levelPathHandler $ baseDir `Text.append` x `Text.append` fileExt
        drawFiles font xs (p+h) (-1)
      drawFiles font (x:xs) p i = do
        surf <- Font.solid font (V4 255 255 255 255) x
        text <- createTextureFromSurface renderer surf
        tInfo <- queryTexture text
        let w = textureWidth tInfo
        let h = textureHeight tInfo
        SDL.copy renderer text Nothing (Just (Rectangle (P (V2 0 p)) (V2 w h)))
        drawFiles font xs (p+h) (i-1)

fontSize = 20

