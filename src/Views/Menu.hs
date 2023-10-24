module Views.Menu (renderMenu) where
import           Graphics.Gloss
import           Model
import           Utils.Render

renderMenu :: GameState -> Picture
renderMenu _ = Pictures [title, sp, mp]
  where
    title = renderText "Asteroids" (-125) 100 0.5
    sp = Pictures [
        renderText "A - Singleplayer" (-100) 50 0.2,
        translate 0 0 $ renderSpaceShip red
      ]
    mp = Pictures [
        renderText "D - Multiplayer" (-100) (-80) 0.2,
        translate (-25) (-130) $ Pictures [
          renderSpaceShip red,
          translate 50 0 $ renderSpaceShip yellow
        ]
      ]
