module Controllers.GameOver (gameOverKeys) where
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Model
import           Models.ModelLib

gameOverKeys :: Event -> GameState -> GameState
gameOverKeys (EventKey (Char 'r') Down _ _) gs = newGame gs (mode gs)
gameOverKeys (EventKey (Char 'm') Down _ _) gs = gs { screen = Menu }
gameOverKeys _ gs                              = gs
