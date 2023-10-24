module Main where

import           Controllers.Controller
import           Graphics.Gloss.Interface.IO.Game
import           Models.Model
import           Models.ModelLib
import           System.Random
import           Views.View

main :: IO ()
main = do
  seed <- getStdGen
  playIO
    window
    background
    frames
    initialState { stdGen = mkStdGen 100 }
    render
    handleKeys
    update
