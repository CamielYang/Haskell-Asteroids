module View where

import           Graphics.Gloss
import           Model

window :: Display
window = InWindow "Asteroids" (400, 400) (100, 100)

background :: Color
background = white

frames :: Int
frames = 10

render :: GameState -> IO Picture
render _ = return (
  Pictures [
    circle 80
  ])
