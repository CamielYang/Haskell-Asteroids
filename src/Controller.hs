module Controller where

import           Graphics.Gloss.Interface.IO.Game
import           Model

update :: Float -> GameState -> IO GameState
update _ = return

handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyEsc) Down _ _) gameState = do
  return gameState { status = toggleStatus (status gameState) }
  where
    toggleStatus Active = Paused
    toggleStatus Paused = Active
    toggleStatus s      = s
handleKeys _ gameState = return gameState

