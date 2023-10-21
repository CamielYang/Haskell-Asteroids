module Main where

import           Controllers.Controller
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           Views.View

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
