module Views.Game (renderGame) where
import           Graphics.Gloss
import           Model
import           Utils.Render

renderGame :: GameState -> IO Picture
renderGame gs = return (
  Pictures [
      renderPlayer p1 red,
      if isMp then renderPlayer p2 yellow else blank,
      renderProjectiles,
      renderAsteroids,
      title,
      renderScore,
      renderHpP1,
      renderHpP2
    ]
  )
  where
    title = renderText (show $ mode gs) (-75) (windowTop - 40) 0.2 0.2
    isMp = mode gs == Multiplayer
    p1 = playerOne gs
    p2 = playerTwo gs
    renderScore = renderText (show $ score gs) (-50) (windowBottom + 25) 0.2 0.2
    renderHp c = scale 0.5 0.5 $ renderSpaceShip c
    renderHpP1 = Pictures [
        translate (windowLeft + (25 * fromIntegral hp)) (windowTop - 25)
        $ renderHp red
        | hp <- [1..getHp p1]
      ]
    renderHpP2 = if isMp
      then Pictures [
        translate (windowRight - (25 * fromIntegral hp)) (windowTop - 25)
        $ renderHp yellow
        | hp <- [1..getHp p2]
      ]
      else blank
    renderPlayer p c = translate x' y'
      $ rotate (fromIntegral $ getRotation p) $ renderSpaceShip c
      where
        Pos (Vector2 x' y') = position p
    renderProjectiles = Pictures $ map (\(Projectile (Pos (Vector2 x' y')) _) ->
      translate x' y' $ color white $ circleSolid 2) $ projectiles $ world gs
    renderAsteroids = Pictures $ map (\(Asteroid p (Pos (Vector2 x' y')) _) ->
      translate x' y' $ renderAsteroid p) $ asteroids $ world gs
