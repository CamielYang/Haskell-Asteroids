module Controller where

import           Graphics.Gloss.Interface.IO.Game
import           Model

update :: Float -> GameState -> IO GameState
update _ = return

handleKeys :: Event -> GameState -> IO GameState
handleKeys e gameState
  | currentStatus == Active = return (gameActiveKeys e gameState)
  | otherwise               = return (gameOverKeys e gameState)
  where
    currentStatus = status gameState

gameActiveKeys :: Event -> GameState -> GameState
gameActiveKeys (EventKey (SpecialKey KeyEsc) Down _ _) gameState =
  gameState { status = toggleStatus (status gameState) }
  where
    toggleStatus Active = Paused
    toggleStatus Paused = Active
    toggleStatus s      = s
gameActiveKeys _ gameState = gameState

gameOverKeys :: Event -> GameState -> GameState
gameOverKeys _ gameState = gameState

