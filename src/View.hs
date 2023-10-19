module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           Screens.Game
import           Screens.Menu
import           ViewLib

import qualified Data.Set                         as S

import qualified Data.Set                         as S

window :: Display
window = InWindow "Asteroids" (width, height) (100, 100)

background :: Color
background = makeColorI 36 28 65 255

frames :: Int
frames = 30

render :: GameState -> IO Picture
render gs
  | currentScreen == Menu = renderMenu gs
  | currentScreen == InGame = renderInGame gs
  | otherwise = renderMenu gs
  where
    currentScreen = screen gs

-- renderGameOver :: GameState -> IO Picture
