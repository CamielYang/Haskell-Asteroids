module Controllers.Game (updateGame, gameKeys) where
import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Model
import           System.Random                      (StdGen)
import           Utils.Collision
import           Utils.Keys
import           Utils.Lib
import           Utils.PathModels
import           Utils.Random
import           Utils.Render

updatePosition :: Player -> Position
updatePosition (Player { position = Pos pVec, velocity = Vel vVec }) = Pos (pVec + vVec)

updateRotation :: Player -> GameState -> Key -> Key -> Rotation
updateRotation p gs l r
  | S.member l (keys gs) = setRotation p (-rotationSpeed)
  | S.member r (keys gs) = setRotation p rotationSpeed
  | otherwise            = rotation p

updateVelocity :: Player -> GameState -> Key -> Key -> Velocity
updateVelocity p gs u d
  | S.member u (keys gs) = setVelocity p force
  | S.member d (keys gs) = setVelocity p (-force / 2)
  | otherwise            = setVelocity p 0

updateHealth :: Player -> GameState -> Health
updateHealth p gs
  | shipCollided p gs && getCooldown p <= 0 = HP (getHp p - 1)
  | getHp p <= 0                            = HP 0
  | otherwise                               = health p

updateCooldown :: Player -> Float -> GameState -> Timer
updateCooldown p delta gs
  | shipCollided p gs = Time 3
  | getCooldown p > 0 = Time $ getCooldown p - delta
  | otherwise         = Time 0

handleShoot :: Player -> GameState -> Key -> [Projectile]
handleShoot p gs s
  | S.member s (keys gs) = Projectile (Pos (newPosVec + dirVec)) r (Time projectileLifeTime) : ps
  | otherwise            = ps
  where
    ps            = projectiles (world gs)
    Pos newPosVec = updatePosition p
    dirVec        = degreeToVector rot * Vec2 shootDistance shootDistance
    r@(Rot rot)   = rotation p

updatePlayer :: (Player -> GameState)
              -> Float
              -> GameState
              -> Player
              -> Key
              -> Key
              -> Key
              -> Key
              -> Key
              -> GameState
updatePlayer f dt gs p u d l r s = do
  (f newPlayer) {
    world = (world gs) {
      projectiles = handleShoot p gs s
    }
  }
  where
    newPlayer = p {
      position = updatePosition p
    , rotation = updateRotation p gs l r
    , velocity = updateVelocity p gs u d
    , health   = updateHealth p gs
    , cooldown = updateCooldown p dt gs
    }

updateProjectiles :: Float -> GameState -> GameState
updateProjectiles _ gs@(GameState { world = World { projectiles = [] } }) = gs
updateProjectiles d gs@(GameState { world = World { projectiles = ps, asteroids = as } }) =
  gs {
    world = (world gs) { projectiles = map func filtered },
    score = if (not . all notCollided) ps
            then addScore (score gs)
            else score gs
  }
  where
    filtered                                      = filter (\p -> notAged p && notCollided p) ps
    notCollided p                                 = not (any (projectileCollided p) as)
    notAged (Projectile _ _ (Time t))             = t > 0
    func (Projectile (Pos pVec) (Rot r) (Time t)) = Projectile (Pos (pVec + dirVec)) (Rot r) (Time $ t - d)
      where
        dirVec = degreeToVector r * Vec2 projectileSpeed projectileSpeed

randomAsteroid :: StdGen -> (Asteroid, StdGen)
randomAsteroid gen = (Asteroid path (Pos (Vec2 x' y')) (Rot rot), gen4)
  where
    (path, gen1) = asteroidPath gen
    (rot, gen2)  = randomInt 0 360 gen1
    (x', gen3)   = randomFloat windowLeft windowRight gen2
    (y', gen4)   = randomFloat windowBottom windowTop gen3

splitAsteroid :: Asteroid -> StdGen -> (Asteroid, StdGen)
splitAsteroid (Asteroid p (Pos posVec') _) gen = (Asteroid path' (Pos (posVec' + Vec2 dX dY)) (Rot dR), gen4)
  where
    (dX, gen1)    = randomFloat 0 10 gen
    (dY, gen2)    = randomFloat 0 10 gen1
    (dR, gen3)    = randomInt 0 360 gen2
    (path', gen4) = asteroidPathScaled size size gen3
    size          = largestRadius p / 3

updateAsteroids :: GameState -> StdGen -> ([Asteroid], StdGen)
updateAsteroids (GameState { world = World { asteroids = as, projectiles = ps } }) gen
  | length as < 10 = (newAsteroid : newAs, gen2)
  | otherwise      = (newAs, gen2)
  where
    (mapped, gen1)      = mapRandom func as gen
    newAs               = concat mapped
    (newAsteroid, gen2) = randomAsteroid gen1
    func :: Asteroid -> StdGen -> ([Asteroid], StdGen)
    func asteroid@(Asteroid path (Pos posVec) (Rot r)) gen'
      | collided && ld >= 30 = ([a1,a2], gen2')
      | collided && ld < 30  = ([], gen')
      | otherwise            = ([Asteroid path (Pos (posVec + degreeToVector r)) (Rot r)], gen')
      where
        (a1, gen1') = splitAsteroid asteroid gen
        (a2, gen2') = splitAsteroid asteroid gen1'
        ld          = largestRadius path
        collided    = any (`projectileCollided` asteroid) ps

updateWorld :: Float -> GameState -> GameState
updateWorld d gs = newGs {
    world = (world newGs) {
      asteroids = newAs,
      powerUps  = []
    },
    stdGen = newGen
  }
  where
    (newAs, newGen) = updateAsteroids gs (stdGen gs)
    newGs           = updateProjectiles d gs

-- obtainPowerUp :: PowerUpType -> Player -> Player
-- obtainPowerUp (Heart n) player = player { health = HP (n + getHealth (health player)) }
--   where
--     getHealth :: Health -> Int
--     getHealth (HP v) = v
-- obtainPowerUp (Weapon weaponType) player = player { weapon = weaponType }

updateGame :: Float -> GameState -> GameState
updateGame d gs = gs2 {
    keys = disableKeys (keys gs2) [SpecialKey KeyEnter, SpecialKey KeySpace]
  }
  where
    newGs = updateWorld d gs
    gs1 = updatePlayer
            (\p -> newGs { playerOne = p })
            d
            newGs
            (playerOne newGs)
            (Char 'w')
            (Char 's')
            (Char 'a')
            (Char 'd')
            (SpecialKey KeySpace)
    gs2 = if mode gs == Multiplayer
          then
          updatePlayer
            (\p -> gs1 { playerTwo = p })
            d
            gs1
            (playerTwo gs1)
            (SpecialKey KeyUp)
            (SpecialKey KeyDown)
            (SpecialKey KeyLeft)
            (SpecialKey KeyRight)
            (SpecialKey KeyEnter)
          else gs1

gameKeys :: Event -> GameState -> GameState
gameKeys (EventKey (SpecialKey KeyEsc) Down _ _) gs = gs { screen = Pause }
gameKeys (EventKey k Down _ _) gs                   = gs { keys = S.insert k (keys gs)}
gameKeys (EventKey k Up _ _) gs                     = gs { keys = S.delete k (keys gs)}
gameKeys _ gs                                       = gs
