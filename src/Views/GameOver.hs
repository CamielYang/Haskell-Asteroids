module Views.GameOver (renderGameOver) where
import           Graphics.Gloss
import           Model
import           Utils.Render
import           Views.Game

renderGameOver :: GameState -> Picture
renderGameOver gs = Pictures [renderGame gs, renderOverlay]
  where
    renderOverlay = Pictures [
        Color (makeColorI 0 0 0 75) $ rectangleSolid width height,
        renderText "Game Over" (-175) 100 0.5,
        renderText "R - Restart" (-100) 50 0.2,
        renderText "M - Main menu" (-125) 0 0.2
      ]