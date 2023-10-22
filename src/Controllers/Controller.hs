module Controllers.Controller (update, handleKeys) where

import           Graphics.Gloss.Interface.IO.Game
import           Model

import           Controllers.Game
import           Controllers.Menu

update :: Float -> GameState -> IO GameState
update d gs
  | currentScreen == InGame = updateGame d gs
  | otherwise               = return gs
  where
    currentScreen = screen gs

handleKeys :: Event -> GameState -> IO GameState
handleKeys e gs
  | currentScreen == Menu   = return (menuKeys e gs)
  | currentScreen == InGame = return (gameKeys e gs)
  | otherwise               = return gs
  where
    currentScreen = screen gs
