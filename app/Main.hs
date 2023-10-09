module Main (main) where

import           Controller
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           View

main :: IO ()
main = do
  playIO
    window
    background
    frames
    initialState
    render
    handleKeys
    update
