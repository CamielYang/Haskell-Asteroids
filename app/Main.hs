module Main where

import           Controllers.Controller
import           Graphics.Gloss.Interface.Pure.Game
import           Model
import           System.Random
import           Views.View

main :: IO ()
main = do
  seed <- getStdGen
  play
    window
    background
    frames
    initialState { stdGen = seed }
    render
    handleKeys
    update
