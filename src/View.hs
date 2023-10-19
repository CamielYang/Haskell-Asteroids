module View where

import           Graphics.Gloss
import           Model
import           Screens.Game
import           Screens.Menu
import           ViewLib

import qualified Data.Set       as S

window :: Display
window = InWindow "Asteroids" (400, 400) (100, 100)

background :: Color
background = white

frames :: Int
frames = 10

render :: GameState -> IO Picture
render gs
  | currentScreen == Menu = renderMenu gs
  | currentScreen == InGame = renderInGame gs
  | otherwise = renderMenu gs
  where
    currentScreen = screen gs

-- renderGameOver :: GameState -> IO Picture
