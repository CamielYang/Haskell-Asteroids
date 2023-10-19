module Main where

import           Controller
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           View
import           ViewLib

main :: IO ()
main = do
  t <- renderAsteroid AsteroidLg
  playIO
    window
    background
    frames
    initialState { world = World { asteroids = [Asteroid AsteroidLg t (Pos Vector2 { x = 0, y = 0 }) (Vel Vector2 { x = 0, y = 0 })]}  }
    render
    handleKeys
    update
