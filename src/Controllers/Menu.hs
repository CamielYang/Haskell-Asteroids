module Controllers.Menu (menuKeys) where
import           Graphics.Gloss.Interface.Pure.Game
import           Model

menuKeys :: Event -> GameState -> GameState
menuKeys (EventKey (Char 'a') Down _ _) gs = initialState { screen = InGame, mode = Singleplayer, stdGen = stdGen gs }
menuKeys (EventKey (Char 'd') Down _ _) gs = initialState { screen = InGame, mode = Multiplayer, stdGen = stdGen gs }
menuKeys _ gameState                       = gameState
