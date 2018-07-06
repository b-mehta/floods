{-# LANGUAGE RecordWildCards #-}
module Main
  where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Environment                       (lookupEnv)
import Data.Maybe                               (maybe)
import Control.Monad                            (replicateM, void)
import Data.IORef                               (modifyIORef', newIORef, readIORef)
import Data.Array.IArray                        ((!))
import Data.Char                                (toLower)
import Control.Monad.Trans                      (liftIO)
import Control.Concurrent                       (forkIO)
import Control.Exception                        (evaluate)

import Grid                                     (flood1, Grid(..))
import Colour                                   (Colour, randomBoardIO)
import Solve                                    

main :: IO ()
main = do
  port <- maybe 10000 read <$> lookupEnv "PORT"
  putStrLn "running..."
  startGUI defaultConfig {jsPort = Just port, jsStatic = Just "src/static"} setup

strategies :: Value a => [Strategy a]
strategies = [greedyArea, cycleColour]

startGame :: Int -> UI Element
startGame n = do
  liftIO $ putStrLn "starting game"
  game <- liftIO $ newIORef =<< randomBoardIO n n
  score <- liftIO $ newIORef 0

  let num = floor (540/fromIntegral n)
      leng = show num ++ "px"
  elBoard <- replicateM n . replicateM n
    $ UI.new # set style [("width",leng), ("height",leng)]
  elGrid <- grid (map (map element) elBoard)

  elBorder <- UI.new #. "inner-center"
    # set style [("padding","10px")]
    # set children [elGrid]

  let scoreMaker :: Strategy Colour -> UI Element
      scoreMaker Strategy{..} = do
        tooltip <- string name #. "tooltiptext"
        label <- string "CPU score: "

        computerScore <- string "working..."

        liftIO $ forkIO $ do
          g <- evaluate =<< solution play <$> readIORef game
          w <- getWindow computerScore
          runUI w $ void $ do
            element computerScore # set text (show g)

        UI.new #. "score tooltip"
               # set children [label, computerScore, tooltip]
               # set style [("color", "grey")]

  yourScore <- UI.new #. "score"
  cpuScores <- traverse scoreMaker strategies

  scoreBox <- UI.new #. "inner-center"
                     #+ [column (element <$> (yourScore:cpuScores))]

  let redrawGrid :: UI ()
      redrawGrid = do
        Grid b <- liftIO $ readIORef game
        sequence_ [element cell # setColour (b ! (i,j)) | (j,r) <- zip [1..] elBoard, (i,cell) <- zip [1..] r]

        s <- liftIO $ readIORef score
        element yourScore # set text ("Your score: " ++ show s)
        return ()

      mkSelector :: Colour -> UI Element
      mkSelector colour = do
        cell <- UI.new #. ("inner-center selector " ++ (toLower <$> show colour))
                       # setColour colour
        on UI.mousedown cell $ \_ -> do
          liftIO (modifyIORef' game (flood1 colour))
          liftIO (modifyIORef' score (+1))
          redrawGrid
        return cell

  selectors <- traverse mkSelector [minBound..maxBound]
  selectorBox <- UI.new # set children selectors

  gameBox <- UI.new #. "inner-center game-box"
    # set children [elBorder, selectorBox, scoreBox]
  redrawGrid

  return gameBox

setColour :: Colour -> UI Element -> UI Element
setColour col = set style [("background", show col)]

setup :: Window -> UI ()
setup window = do
  liftIO $ putStrLn "starting setup"
  UI.addStyleSheet window "flood.css"
  return window # set title "Flood it!"

  options <- sequence [UI.option # set value (show i) # set text (show i) | i <- [2..30]]

  elNum <- UI.select #. "inner-center"
    # set style [("font-size", "20px")]
    # set children options

  elSend <- UI.button #. "inner-center"
    # set style [("font-size", "20px"), ("margin","20px"), ("cursor", "pointer")]
    # set text "Start!"

  elEntry <- UI.new #. "center-div"
    # set children [elNum, elSend]

  elGame <- UI.new #. "center-div"

  getBody window
    #+ [element elEntry, element elGame]

  on UI.click elSend $ \_ -> do
    num <- elNum # get value
    gameBox <- startGame (read num)
    element elGame
      # set children [gameBox]
