module Views.Pause (renderPause) where
import           Graphics.Gloss
import           Model
import           Utils.Render
import           Views.Game

renderPause :: GameState -> Picture
renderPause gs = Pictures [renderGame gs, renderOverlay]
  where
    renderOverlay = Pictures [
        Color (makeColorI 0 0 0 75) $ rectangleSolid (fromIntegral width) (fromIntegral height),
        renderText "Paused" (-125) 100 0.5,
        renderText "Esc - Continue" (-120) 50 0.2,
        renderText "R - Restart" (-100) 0 0.2,
        renderText "M - Main menu" (-125) (-50) 0.2
      ]