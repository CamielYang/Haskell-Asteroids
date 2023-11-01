module Views.Menu (renderMenu) where
import           Graphics.Gloss
import           Models.Model
import           Models.Renderable
import           Utils.Render

renderMenu :: GameState -> Picture
renderMenu gs = Pictures [title, sp, mp]
  where
    title = renderText "Asteroids" (-125) 100 0.5
    sp = Pictures [
        renderText "A - Singleplayer" (-100) 50 0.2,
        translate 0 0 $ getPicture $ playerOne gs
      ]
    mp = Pictures [
        renderText "D - Multiplayer" (-100) (-80) 0.2,
        translate (-25) (-130) $ Pictures [
          getPicture $ playerOne gs,
          translate 50 0 $ getPicture $ playerTwo gs
        ]
      ]
