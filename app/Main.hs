module Main where

import           Controller
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           View
import           Utils.ViewLib
import           Utils.Path

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
