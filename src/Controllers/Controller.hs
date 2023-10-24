module Controllers.Controller (update, handleKeys) where

import           Graphics.Gloss.Interface.Pure.Game
import           Models.Model

import           Controllers.Game
import           Controllers.GameOver
import           Controllers.Menu
import           Controllers.Pause

update :: Float -> GameState -> IO GameState
update d gs
  | currentScreen == InGame = return $ updateGame d gs
  | otherwise               = return gs
  where
    currentScreen = screen gs

handleKeys :: Event -> GameState -> IO GameState
handleKeys e gs
  | currentScreen == Menu     = return $ menuKeys e gs
  | currentScreen == InGame   = return $ gameKeys e gs
  | currentScreen == Pause    = return $ pauseKeys e gs
  | currentScreen == GameOver = return $ gameOverKeys e gs
  | otherwise                 = return gs
  where
    currentScreen = screen gs
