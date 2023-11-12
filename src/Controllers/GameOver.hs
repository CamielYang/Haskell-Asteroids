module Controllers.GameOver (
  gameOverKeys,
  updateGameOver
) where
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Model
import           Models.ModelLib

gameOverKeys :: Event -> GameState -> GameState
gameOverKeys (EventKey (Char 'r') Down _ _) gs = newGame gs (mode gs)
gameOverKeys (EventKey (Char 'm') Down _ _) gs = gs { screen = Menu }
gameOverKeys _ gs                              = gs

updateGameOver :: GameState -> IO GameState
updateGameOver gs@(GameState { score = Score s, highscore = Highscore hs (Updated False) }) = do
  if hs < s
    then do
      writeFile "data/highscore.txt" (show (getScore gs))
      return $ gs { highscore = Highscore (getScore gs) (Updated True) }
    else
      return $ gs { highscore = Highscore hs (Updated False) }
updateGameOver gs = return gs
