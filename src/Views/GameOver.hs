module Views.GameOver (renderGameOver) where
import           Graphics.Gloss
import           Models.Model
import           Utils.Render
import           Views.Game

renderGameOver :: GameState -> Picture
renderGameOver gs@(GameState { score = Score s, highscore = Highscore hs (Updated u) }) = Pictures [renderGame gs, renderOverlay]
  where
    renderOverlay = Pictures [
        Color (makeColorI 0 0 0 75) $ rectangleSolid width height,
        renderText "Game Over" (-175) 100 0.5,
       renderText "R - Restart" (-80) 50 0.2,
        renderText "M - Main menu" (-105) 0 0.2,
        if hs == s && u then renderText "New Highscore!" (-100) (-100) 0.2 else blank,
        renderText ("Highscore: " ++ show hs) (-80) (-150) 0.2
      ]
