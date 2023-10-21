module Controllers.Game (updateGame, gameKeys) where
import qualified Data.Set                         as S
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           Utils.Collision
import           Utils.Keys
import           Utils.Lib
import           Utils.PathModels
import           Utils.Point
import           Utils.Random
import           Utils.Render

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

updateProjectiles :: GameState -> GameState
updateProjectiles gs@(GameState { world = World { projectiles = [] } }) = gs
updateProjectiles gs@(GameState { world = World { projectiles = ps } }) =
  gs {
    world = (world gs) { projectiles = map func filtered },
    score = if length ps /= length filtered then addScore (score gs) else score gs
  }
  where
    filtered = filter notCollided ps
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
  let newAs = concat mapped

  if length as < 10
    then do
      newAsteroid <- asteroidPath
      rot <- randomInt 0 360
      x <- randomFloat windowLeft windowRight
      y <- randomFloat windowBottom windowTop
      return $ Asteroid newAsteroid (Pos (Vector2 x y)) (Rot rot) : newAs
    else return newAs
  where
    func :: Asteroid -> IO [Asteroid]
    func asteroid@(Asteroid path (Pos (Vector2 x' y')) (Rot r))
      | collided && ld >= 30 = do
          a1 <- splitAsteroid asteroid
          a2 <- splitAsteroid asteroid
          return [a1,a2]
      | collided && ld < 30 = return []
      | otherwise = return [Asteroid path (Pos (Vector2 (x' + xInc) (y' + yInc))) (Rot r)]
      where
        splitAsteroid :: Asteroid -> IO Asteroid
        splitAsteroid (Asteroid _ (Pos (Vector2 x'' y'')) _) = do
          drawX <- randomFloat 0 10
          drawY <- randomFloat 0 10
          drawR <- randomInt 0 360
          path' <- asteroidPath
          return $ Asteroid (scalePath 0.5 path') (Pos (Vector2 (x'' + drawX) (y'' + drawY))) (Rot drawR)
        ld = largestDistance path
        collided = any check ps
        check (Projectile (Pos (Vector2 x'' y'')) _) = circleCollision (x'', y'') (x', y') [(1,1)] path
        Vector2 { x = xInc, y = yInc } = degreeToVector r

updateWorld :: GameState -> IO GameState
updateWorld gs = do
  newAs <- updateAsteroids gs
  let newGs = updateProjectiles gs

  return newGs {
    world = (world newGs) {
      asteroids = newAs,
      powerUps = []
    }
  }

-- obtainPowerUp :: PowerUpType -> Player -> Player
-- obtainPowerUp (Heart n) player = player { health = HP (n + getHealth (health player)) }
--   where
--     getHealth :: Health -> Int
--     getHealth (HP v) = v
-- obtainPowerUp (Weapon weaponType) player = player { weapon = weaponType }

updateGame :: Float -> GameState -> IO GameState
updateGame _ gs = do
  -- traceShow (translatePath (getPos $ playerOne gs) shipPath) (return ())
  -- traceShow shipPath (return ())
  newGs <- updateWorld gs
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

gameKeys :: Event -> GameState -> GameState
gameKeys (EventKey (SpecialKey KeyEsc) Down _ _) gameState =
  gameState { status = toggleStatus (status gameState) }
  where
    toggleStatus Active = Paused
    toggleStatus Paused = Active
gameKeys (EventKey k Down _ _) gameState = gameState { keys = S.insert k (keys gameState)}
gameKeys (EventKey k Up _ _) gameState = gameState { keys = S.delete k (keys gameState)}
gameKeys _ gameState = gameState
