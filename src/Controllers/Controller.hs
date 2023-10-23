module Controllers.Controller (update, handleKeys) where

import           Graphics.Gloss.Interface.Pure.Game
import           Model

import           Controllers.Game
import           Controllers.Menu

update :: Float -> GameState -> GameState
update d gs
  | currentScreen == InGame = updateGame d gs
  | otherwise               = gs
  where
    currentScreen = screen gs

handleKeys :: Event -> GameState -> GameState
handleKeys e gs
  | currentScreen == Menu   = menuKeys e gs
  | currentScreen == InGame = gameKeys e gs
  | otherwise               = gs
  where
    currentScreen = screen gs
