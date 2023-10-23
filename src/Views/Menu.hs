module Views.Menu (renderMenu) where
import           Graphics.Gloss
import           Model
import           Utils.Render

renderMenu :: GameState -> Picture
renderMenu _ = Pictures [title, sp, mp]
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
