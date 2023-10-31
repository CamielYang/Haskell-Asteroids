module Controllers.Pause (pauseKeys) where
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Model
import           Models.ModelLib

pauseKeys :: Event -> GameState -> GameState
pauseKeys (EventKey (SpecialKey KeyEsc) Down _ _) gs = gs { screen = InGame }
pauseKeys (EventKey (Char 'r') Down _ _) gs          = newGame gs (mode gs)
pauseKeys (EventKey (Char 'm') Down _ _) gs          = gs { screen = Menu }
pauseKeys _ gs                                       = gs
