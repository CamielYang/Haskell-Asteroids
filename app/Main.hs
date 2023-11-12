module Main where

import           Controllers.Controller
import           Data.Maybe
import           Graphics.Gloss.Interface.IO.Game
import           Models.Model
import           Models.ModelLib
import           System.IO
import           System.Random
import           Text.Read
import           Views.View

main :: IO ()
main = do
  hs <- readFile' "data/highscore.txt"
  seed <- getStdGen

  playIO
    window
    background
    frames
    initialState {
      stdGen = seed,
      highscore = Highscore (fromMaybe 0 (readMaybe hs)) (Updated False)
    }
    render
    handleKeys
    update
