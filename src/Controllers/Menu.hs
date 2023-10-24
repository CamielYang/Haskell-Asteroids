module Controllers.Menu (menuKeys) where
import           Graphics.Gloss.Interface.Pure.Game
import           Model

menuKeys :: Event -> GameState -> GameState
menuKeys (EventKey (Char 'a') Down _ _) gs = newGame gs Singleplayer
menuKeys (EventKey (Char 'd') Down _ _) gs = newGame gs Multiplayer
menuKeys _ gs                              = gs
