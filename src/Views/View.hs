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
import           Views.GameOver
import           Views.Menu
import           Views.Pause

window :: Display
window = InWindow "Asteroids" (round width, round height) (0, 0)

background :: Color
background = makeColorI 36 28 65 255

frames :: Int
frames = 30

render :: GameState -> IO Picture
render gs
  | currentScreen == Menu     = return $ renderMenu gs
  | currentScreen == InGame   = return $ renderGame gs
  | currentScreen == Pause    = return $ renderPause gs
  | currentScreen == GameOver = return $ renderGameOver gs
  | otherwise                 = return $ renderMenu gs
  where
    currentScreen = screen gs
