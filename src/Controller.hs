module Controller where

-- import           Debug.Trace                      (trace, traceShow)
import           Graphics.Gloss.Interface.IO.Game
import           Model

import           Screens.Game
import           Screens.Menu                     (menuKeys)

update :: Float -> GameState -> IO GameState
update d gs
  | currentScreen == InGame = updateGame d gs
  | otherwise = return gs
  where
    currentScreen = screen gs

handleKeys :: Event -> GameState -> IO GameState
handleKeys e gameState
  | currentScreen == Menu = return (menuKeys e gameState)
  | currentScreen == InGame = return (inGameKeys e gameState)
  | otherwise = return (gameOverKeys e gameState)
  where
    currentScreen = screen gameState

gameOverKeys :: Event -> GameState -> GameState
gameOverKeys _ gameState = gameState

