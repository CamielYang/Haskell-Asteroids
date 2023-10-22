module Controllers.Menu (menuKeys) where
import           Graphics.Gloss.Interface.IO.Game
import           Model

menuKeys :: Event -> GameState -> GameState
menuKeys (EventKey (Char 'a') Down _ _) _  = initialState { screen = InGame, mode = Singleplayer }
menuKeys (EventKey (Char 'd') Down _ _) _  = initialState { screen = InGame, mode = Multiplayer }
menuKeys _ gameState                       = gameState
