module View where

import           Graphics.Gloss
import           Model
import           Screens.Game
import           Screens.Menu
import           Utils.ViewLib

window :: Display
window = InWindow "Asteroids" (width, height) (100, 100)

background :: Color
background = makeColorI 36 28 65 255

frames :: Int
frames = 30

render :: GameState -> IO Picture
render gs
  | currentScreen == Menu = renderMenu gs
  | currentScreen == InGame = renderGame gs
  | otherwise = renderMenu gs
  where
    currentScreen = screen gs

-- renderGameOver :: GameState -> IO Picture
