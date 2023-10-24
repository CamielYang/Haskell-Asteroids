module Views.Game (renderGame) where
import           Graphics.Gloss
import           Model
import           Utils.Render

renderProjectiles :: [Projectile] -> Picture
renderProjectiles = Pictures . map (\(Projectile (Pos (Vec2 x' y')) _ _) ->
  translate x' y' $ color white $ circleSolid 2)

renderAsteroids :: [Asteroid] -> Picture
renderAsteroids = Pictures . map (\(Asteroid p (Pos (Vec2 x' y')) _) ->
  translate x' y' $ renderAsteroid p)

renderPlayer :: Player -> Color -> Picture
renderPlayer (Player { position = Pos (Vec2 x' y'), rotation = Rot r }) c =
  translate x' y'
  $ rotate (fromIntegral r)
  $ color c
  $ renderSpaceShip c

renderHp :: (Float -> Float) -> Player -> Color -> Picture
renderHp f p c = Pictures [
    translate (f (fromIntegral hp)) (windowTop - 25)
    $ hpPicture c
    | hp <- [1..getHp p]
  ]
  where
    hpPicture c = scale 0.5 0.5 $ renderSpaceShip c

renderGame :: GameState -> Picture
renderGame gs = Pictures [
      -- Players
      renderPlayer p1 (determinePlayerColor p1 red),
      isMp $ renderPlayer p2 (determinePlayerColor p2 yellow),

      -- World
      renderProjectiles $ getProjectiles gs,
      renderAsteroids $ getAsteroids gs,

      -- UI
      title,
      renderScore,
      renderHp (\x -> windowLeft + 25 * x) p1 red,
      isMp $ renderHp (\x -> windowRight - 25 * x) p2 yellow
    ]
  where
    title = renderText (show $ mode gs) (-75) (windowTop - 40) 0.2 0.2
    renderScore = renderText (show $ score gs) (-50) (windowBottom + 25) 0.2 0.2
    p1 = playerOne gs
    p2 = playerTwo gs
    isMp p
      | mode gs == Multiplayer = p
      | otherwise              = blank
