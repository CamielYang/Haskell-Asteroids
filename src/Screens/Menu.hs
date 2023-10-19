module Screens.Menu (menuKeys, renderMenu) where
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           ViewLib

-- Controller
menuKeys :: Event -> GameState -> GameState
menuKeys (EventKey (Char 'a') Down _ _) _ = initialState { screen = InGame, mode = Singleplayer }
menuKeys (EventKey (Char 'd') Down _ _) _ = initialState { screen = InGame, mode = Multiplayer }
menuKeys _ gameState = gameState


-- View
renderMenu :: GameState -> IO Picture
renderMenu _ = return (
    Pictures [
      title,
      sp,
      mp
    ]
  )
  where
    title = renderText "Asteroids" (-125) 100 0.5 0.5
    sp = Pictures [
        renderText "Press A for singleplayer" (-150) 50 0.2 0.2,
        translate 0 0 $ renderSpaceShip red
      ]
    mp = Pictures [
        renderText "Press D for multiplayer" (-150) (-80) 0.2 0.2,
        translate (-25) (-130) $ Pictures [
          renderSpaceShip red,
          translate 50 0 $ renderSpaceShip yellow
        ]
      ]
