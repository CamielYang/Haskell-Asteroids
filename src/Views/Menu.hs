module Views.Menu (renderMenu) where
import           Graphics.Gloss
import           Models.Model
import           Utils.Render

renderMenu :: GameState -> Picture
renderMenu gs = Pictures [title, sp, mp]
  where
    title = renderText "Asteroids" (-125) 100 0.5
    sp = Pictures [
        renderText "A - Singleplayer" (-100) 50 0.2,
        translate 0 0 $ renderSpaceShip (pColor $ playerOne gs)
      ]
    mp = Pictures [
        renderText "D - Multiplayer" (-100) (-80) 0.2,
        translate (-25) (-130) $ Pictures [
          renderSpaceShip (pColor $ playerOne gs),
          translate 50 0 $ renderSpaceShip (pColor $ playerTwo gs)
        ]
      ]
