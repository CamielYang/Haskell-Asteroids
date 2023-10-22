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

force, drag, maxVelocity, projectileSpeed, shootDistance :: Float
rotationSpeed :: Int
force           = 0.3
drag            = 0.98
maxVelocity     = 10
projectileSpeed = 10
rotationSpeed   = 10
shootDistance   = 25

updatePosition :: Player -> Position
updatePosition (Player { position = Pos pVec, velocity = Vel vVec }) = Pos (pVec + vVec)

updateVelocity :: Player -> Float -> Velocity
updateVelocity p@(Player { velocity = Vel vVec }) v = if lengthOfVector newV2 > maxVelocity
  then velocity p
  else Vel newV2
  where
    Rot d = rotation p
    dirVec = degreeToVector d * Vec2 v v
    dragVec = Vec2 drag drag
    newV2 = (vVec + dirVec) * dragVec

shipCollided :: Player -> GameState -> Bool
shipCollided p gs = any check (asteroids $ world gs) && getCooldown p <= 0
  where
    check :: Asteroid -> Bool
    check (Asteroid path (Pos (Vec2 x'' y'')) _) = circleCollision (x'', y'') (x', y') shipPath path
    Pos (Vec2 x' y') = position p

updatePlayer :: Float -> GameState -> Player -> Key -> Key -> Key -> Key -> Key -> (Player, [Projectile])
updatePlayer delta gs p@(Player { rotation = Rot rot }) u d l r s = do
  (p {
      position = newPos
    , rotation = let r' | S.member l (keys gs) = updateRotation p (-rotationSpeed)
                        | S.member r (keys gs) = updateRotation p rotationSpeed
                        | otherwise            = rotation p
                 in r'
    , velocity = let v' | S.member u (keys gs) = updateVelocity p force
                        | S.member d (keys gs) = updateVelocity p (-force / 2)
                        | otherwise            = Vel (velVec * Vec2 drag drag)
                 in v'
    , health   = let h' | getHp p <= 0         = HP 0
                        | otherwise            = health p
                 in h'
    , cooldown = let c' | shipCollided p gs    = 3
                        | getCooldown p > 0    = getCooldown p - delta
                        | otherwise            = 0
                 in c'
    },
    let ps' | S.member s (keys gs) = Projectile (Pos (newPosVec + dirVec)) (Rot rot) : ps
            | otherwise            = projectiles (world gs)
    in ps')
  where
    Vel velVec             = velocity p
    ps                     = projectiles (world gs)
    newPos@(Pos newPosVec) = updatePosition p
    dirVec                 = degreeToVector rot * Vec2 shootDistance shootDistance

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
    notCollided (Projectile (Pos (Vec2 x' y')) _) = not $ any check (asteroids $ world gs)
      where
        check :: Asteroid -> Bool
        check (Asteroid path (Pos (Vec2 x'' y'')) _) = circleCollision (x', y') (x'', y'') [(1,1)] path
    func (Projectile (Pos pVec) (Rot r)) = Projectile (Pos (pVec + dirVec)) (Rot r)
      where
        dirVec = degreeToVector r * Vec2 projectileSpeed projectileSpeed

randomAsteroid :: IO Asteroid
randomAsteroid = do
  path <- asteroidPath
  rot <- randomInt 0 360
  x' <- randomFloat windowLeft windowRight
  y' <- randomFloat windowBottom windowTop
  return $ Asteroid path (Pos (Vec2 x' y')) (Rot rot)

splitAsteroid :: Asteroid -> IO Asteroid
splitAsteroid (Asteroid _ (Pos posVec') _) = do
  dX <- randomFloat 0 10
  dY <- randomFloat 0 10
  dR <- randomInt 0 360
  path' <- asteroidPath
  return $ Asteroid (scalePath 0.5 path') (Pos (posVec' + Vec2 dX dY)) (Rot dR)

updateAsteroids :: GameState -> IO [Asteroid]
updateAsteroids (GameState { world = World { asteroids = as, projectiles = ps } }) = do
  mapped <- mapM func as
  let newAs = concat mapped

  if length as < 10 then do
    newAsteroid <- randomAsteroid
    return $ newAsteroid : newAs
  else return newAs
  where
    func :: Asteroid -> IO [Asteroid]
    func asteroid@(Asteroid path (Pos posVec) (Rot r))
      | collided && ld >= 30 = do
          a1 <- splitAsteroid asteroid
          a2 <- splitAsteroid asteroid
          return [a1,a2]
      | collided && ld < 30 = return []
      | otherwise = return [Asteroid path (Pos (posVec + degreeToVector r)) (Rot r)]
      where
        ld = largestDistance path
        collided = any check ps
        check (Projectile (Pos pVec) _) = circleCollision (v2ToTuple pVec) (x posVec, y posVec) [(1,1)] path

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
updateGame d gs = do
  newGs <- updateWorld gs
  let gs1 = (\(p1, ps) -> newGs {

                playerOne = p1
              , world = (world newGs) { projectiles = ps }
            }) (updatePlayer
                  d
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
                    d
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
gameKeys (EventKey k Up _ _) gameState   = gameState { keys = S.delete k (keys gameState)}
gameKeys _ gameState                     = gameState
