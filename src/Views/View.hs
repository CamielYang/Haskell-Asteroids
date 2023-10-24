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
window = InWindow "Asteroids" (width, height) (0, 0)

background :: Color
background = makeColorI 36 28 65 255

frames :: Int
frames = 30

render :: GameState -> IO Picture
render gs
  | currentScreen == Menu   = return $ renderMenu gs
  | currentScreen == InGame = return $ renderGame gs
  | otherwise               = return $ renderMenu gs
  where
    currentScreen = screen gs
