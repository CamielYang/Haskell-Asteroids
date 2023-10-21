module Screens.Game (updateGame, gameKeys, renderGame) where
import           Control.Monad                    (mapM)
import qualified Data.Set                         as S
import           Debug.Trace
import           GHC.IO.Buffer                    (checkBuffer)
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           Utils.Collision                  (circleCollision,
                                                   largestDistance)
import           Utils.Lib
import           Utils.Path
import           Utils.Point                      (rotatePath, scalePath,
                                                   translatePath)
import           Utils.ViewLib                    (getHp, getRotation,
                                                   renderAsteroid,
                                                   renderSpaceShip, renderText,
                                                   windowBottom, windowLeft,
                                                   windowRight, windowTop)

-- Controller
force, drag, maxVelocity, projectileSpeed :: Float
rotationSpeed :: Int
force = 0.3
drag = 0.98
maxVelocity = 10
projectileSpeed = 10
rotationSpeed = 10

updatePosition :: Player -> Velocity -> Position
updatePosition (Player { position = Pos (Vector2 x' y') }) (Vel (Vector2 vx vy)) = Pos (Vector2 (x' + vx) (y' + vy))

updateVelocity :: Player -> Float -> Velocity
updateVelocity p@(Player { velocity = Vel (Vector2 x' y') }) v = if lengthOfVector newV2 > maxVelocity
  then velocity p
  else Vel newV2
  where
    Rot d = rotation p
    Vector2 xInc yInc = degreeToVector d
    newV2 = Vector2 ((x' + xInc * v) * drag) ((y' + yInc * v) * drag)

updatePlayer :: GameState -> Player -> Key -> Key -> Key -> Key -> Key -> (Player, [Projectile])
updatePlayer gs p@(Player { rotation = Rot rot }) u d l r s = do
  (p {
      rotation = if S.member l (keys gs)
                  then updateRotation p (-rotationSpeed)
                  else if S.member r (keys gs)
                    then updateRotation p rotationSpeed
                    else rotation p
    , position = newPos
    , velocity = if S.member u (keys gs)
                  then updateVelocity p force
                  else if S.member d (keys gs)
                    then updateVelocity p (-force / 2)
                    else Vel Vector2 { x = vx * drag, y = vy * drag }
    },
    if S.member s (keys gs)
        then
          Projectile (Pos (Vector2 (xNew + xInc * 25) (yNew + yInc * 25))) (Rot rot) : ps
        else
          ps)
    where
      Vector2 xInc yInc = degreeToVector rot
      v@(Vel (Vector2 vx vy)) = velocity p
      ps = projectiles (world gs)
      newPos@(Pos (Vector2 xNew yNew)) = updatePosition p v

updateProjectiles :: GameState -> [Projectile]
updateProjectiles (GameState { world = World { projectiles = [] } }) = []
updateProjectiles gs@(GameState { world = World { projectiles = ps } }) =
  map func $ filter notCollided ps
  where
    notCollided :: Projectile -> Bool
    notCollided (Projectile (Pos (Vector2 x' y')) _) = not $ any check (asteroids $ world gs)
      where
        check :: Asteroid -> Bool
        check (Asteroid path (Pos (Vector2 x'' y'')) _) = circleCollision (x', y') (x'', y'') [(1,1)] path
    func (Projectile (Pos (Vector2 x' y')) (Rot r)) = Projectile (Pos (Vector2 (x' + xInc * projectileSpeed) (y' + yInc * projectileSpeed))) (Rot r)
      where
        Vector2 { x = xInc, y = yInc } = degreeToVector r

updateAsteroids :: GameState -> IO [Asteroid]
updateAsteroids (GameState { world = World { asteroids = as, projectiles = ps } }) = do
  mapped <- mapM func as
  let newAt = concat mapped

  if length as < 10
    then do
      newAsteroid <- asteroidPath
      rot <- drawInt 0 360
      return $ Asteroid newAsteroid (Pos (Vector2 0 0)) (Rot rot) : newAt
    else return newAt
  where
    func :: Asteroid -> IO [Asteroid]
    func asteroid@(Asteroid path (Pos (Vector2 x' y')) (Rot r))
      | collided && ld >= 30 = do
          a1 <- randAsteroidMove asteroid
          a2 <- randAsteroidMove asteroid
          return [a1,a2]
      | collided && ld < 30 = return []
      | otherwise = return [Asteroid path (Pos (Vector2 (x' + xInc) (y' + yInc))) (Rot r)]
      where
        randAsteroidMove :: Asteroid -> IO Asteroid
        randAsteroidMove (Asteroid _ (Pos (Vector2 x'' y'')) _) = do
          drawX <- drawFloat 0 10
          drawY <- drawFloat 0 10
          drawR <- drawInt 0 360
          path' <- asteroidPath
          return $ Asteroid (scalePath 0.5 path') (Pos (Vector2 (x'' + drawX) (y'' + drawY))) (Rot drawR)
        ld = largestDistance path
        collided = any check ps
        check (Projectile (Pos (Vector2 x'' y'')) _) = circleCollision (x'', y'') (x', y') [(1,1)] path
        Vector2 { x = xInc, y = yInc } = degreeToVector r

updateWold :: GameState -> IO GameState
updateWold gs = do
  newAs <- updateAsteroids gs
  return gs {
    world = World {
      asteroids = newAs,
      projectiles = updateProjectiles gs,
      powerUps = []
    }
  }


disableKeys :: S.Set Key -> [Key] -> S.Set Key
disableKeys s [] = s
disableKeys s (k:ks)
  | S.member k s = disableKeys (S.delete k s) ks
  | otherwise = disableKeys s ks

gameKeys :: Event -> GameState -> GameState
gameKeys (EventKey (SpecialKey KeyEsc) Down _ _) gameState =
  gameState { status = toggleStatus (status gameState) }
  where
    toggleStatus Active = Paused
    toggleStatus Paused = Active
gameKeys (EventKey k Down _ _) gameState = gameState { keys = S.insert k (keys gameState)}
gameKeys (EventKey k Up _ _) gameState = gameState { keys = S.delete k (keys gameState)}
gameKeys _ gameState = gameState

obtainPowerUp :: PowerUpType -> Player -> Player
obtainPowerUp (Heart n) player = player { health = HP (n + getHealth (health player)) }
  where
    getHealth :: Health -> Int
    getHealth (HP v) = v
obtainPowerUp (Weapon weaponType) player = player { weapon = weaponType }

updateGame :: Float -> GameState -> IO GameState
updateGame _ gs = do
  -- traceShow (translatePath (getPos $ playerOne gs) shipPath) (return ())
  -- traceShow shipPath (return ())
  newGs <- updateWold gs
  let gs1 = (\(p1, ps) -> newGs {

                playerOne = p1
              , world = (world newGs) { projectiles = ps }
            }) (updatePlayer
                  newGs
                  (playerOne newGs)
                  (Char 'w')
                  (Char 's')
                  (Char 'a')
                  (Char 'd')
                  (SpecialKey KeySpace))

  let gs2 = if mode gs == Multiplayer
              then (\(p, ps) -> gs1 {
                  playerTwo = p
                , world = (world gs1) { projectiles = ps }
              }) (updatePlayer
                    gs1
                    (playerTwo gs1)
                    (SpecialKey KeyUp)
                    (SpecialKey KeyDown)
                    (SpecialKey KeyLeft)
                    (SpecialKey KeyRight)
                    (SpecialKey KeyEnter))
              else gs1

  return gs2 {
    keys = disableKeys (keys gs2) [SpecialKey KeyEnter, SpecialKey KeySpace]
  }

getPos :: Player -> Point
getPos (Player { position = Pos (Vector2 x' y') }) = (x', y')


-- View
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
    renderHpP1 = Pictures [translate (windowLeft + (25 * fromIntegral hp)) (windowTop - 25) $ scale 0.5 0.5 $ renderSpaceShip red | hp <- [1..getHp p1]]
    renderHpP2 = if isMp then Pictures [translate (windowRight - (25 * fromIntegral hp)) (windowTop - 25) $ scale 0.5 0.5 $ renderSpaceShip yellow | hp <- [1..getHp p2]] else blank
    renderProjectiles = Pictures $ map (\(Projectile (Pos (Vector2 x' y')) _) ->
      translate x' y' $ color white $ circleSolid 2) $ projectiles $ world gs
    renderPlayer p c = translate x' y' $ rotate (fromIntegral $ getRotation p) $ renderSpaceShip c
      where
        Pos (Vector2 x' y') = position p
    renderAsteroids = Pictures $ map (\(Asteroid p (Pos (Vector2 x' y')) _) ->
      translate x' y' $ renderAsteroid p) $ asteroids $ world gs
