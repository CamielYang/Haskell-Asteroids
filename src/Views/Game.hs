module Views.Game (renderGame) where
import           Graphics.Gloss
import           Models.Model
import           Models.ModelLib
import           Models.Renderable
import           Utils.Render

renderHp :: (Float -> Float) -> Player -> Picture
renderHp f p = Pictures [translate (f (fromIntegral hp)) (windowTop - 25) hpPicture | hp <- [1..getHp p]]
  where
    hpPicture = scale 0.5 0.5 $ getPicture p

renderGame :: GameState -> Picture
renderGame gs = Pictures [
      -- Players
      render p1,
      isMp $ render p2,

      -- World
      renderMap $ getProjectiles gs,
      renderMap $ getAsteroids gs,

      -- UI
      title,
      renderScore,
      renderHp (\x -> windowLeft + 25 * x) p1,
      isMp $ renderHp (\x -> windowRight - 25 * x) p2
    ]
  where
    title = renderText (show $ mode gs) (-75) (windowTop - 40) 0.2
    renderScore = renderText (show $ score gs) (-50) (windowBottom + 25) 0.2
    p1 = playerOne gs
    p2 = playerTwo gs
    isMp p
      | mode gs == Multiplayer = p
      | otherwise              = blank
