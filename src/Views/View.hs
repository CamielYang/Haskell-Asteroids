module Views.View (
  window,
  background,
  frames,
  render
) where

import           Graphics.Gloss
import           Model
import           Utils.Render
import           Views.Game
import           Views.Menu

window :: Display
window = InWindow "Asteroids" (width, height) (100, 100)

background :: Color
background = makeColorI 36 28 65 255

frames :: Int
frames = 30

render :: GameState -> Picture
render gs
  | currentScreen == Menu   = renderMenu gs
  | currentScreen == InGame = renderGame gs
  | otherwise               = renderMenu gs
  where
    currentScreen = screen gs
