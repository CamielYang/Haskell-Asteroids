module Controllers.Game (updateGame, gameKeys) where
import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Collidable
import           Models.Model
import           Models.ModelLib
import           Models.Positioned
import           System.Random                      (StdGen)
import           Utils.Keys
import           Utils.Lib
import           Utils.PathModels
import           Utils.Random
import           Utils.Render

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
handleShoot p@(Player _ r@(Rot rot) _ _ _ _ weaponType _) gs s
  | weaponType == Default && S.member s (keys gs) = Projectile (updatePosition p dirVec) r (Time projectileLifeTime) : ps
  | weaponType == Shotgun && S.member s (keys gs) = weaponShotgun 10: weaponShotgun 0: weaponShotgun (-10): ps
  | weaponType == Rifle && S.member s (keys gs) = weaponRifle 1: weaponRifle 2: weaponRifle 3: ps
  | otherwise  = ps
  where
    ps            = projectiles (world gs)
    dirVec        = degreeToVector rot * Vec2 shootDistance shootDistance
    weaponShotgun deg = Projectile (updatePosition p dirVec) (Rot $ rot + deg) (Time projectileLifeTime)
    weaponRifle deg = Projectile (updatePosition p (dirVec * deg)) r (Time projectileLifeTime)

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
updatePlayer f dt gs p u d l r s
  | isKilled p = gs
  | otherwise = (f newPlayer) {
    world = (world gs) {
      projectiles = handleShoot newPlayer gs s
    }
  }
  where
    newPlayer = p {
      rotation = updateRotation p gs l r
    , position = move p
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
    filtered                               = filter (\p -> notAged p && notCollided p) ps
    notCollided p                          = not (any (isColliding p) as)
    notAged (Projectile _ _ (Time t))      = t > 0
    func p@(Projectile _ (Rot r) (Time t)) = Projectile (move p) (Rot r) (Time $ t - d)

randomAsteroid :: StdGen -> (Asteroid, StdGen)
randomAsteroid gen = (Asteroid path' (Pos (Vec2 x' y')) (Rot rot), gen4)
  where
    (path', gen1) = asteroidPath gen
    (rot, gen2)  = randomInt 0 360 gen1
    (x', gen3)   = randomFloat windowLeft windowRight gen2
    (y', gen4)   = randomFloat windowBottom windowTop gen3

splitAsteroid :: Asteroid -> StdGen -> (Asteroid, StdGen)
splitAsteroid a gen = (Asteroid path' (updatePosition a (Vec2 dX dY)) (Rot dR), gen4)
  where
    (dX, gen1)    = randomFloat 0 10 gen
    (dY, gen2)    = randomFloat 0 10 gen1
    (dR, gen3)    = randomInt 0 360 gen2
    (path', gen4) = asteroidPathScaled size size gen3
    size          = getHitboxRadius a / 3

updateAsteroids :: GameState -> StdGen -> ([Asteroid], StdGen)
updateAsteroids (GameState { world = World { asteroids = as, projectiles = ps } }) gen
  | length as < 10 = (newAsteroid : newAs, gen2)
  | otherwise      = (newAs, gen2)
  where
    (mapped, gen1)      = mapRandom func as gen
    newAs               = concat mapped
    (newAsteroid, gen2) = randomAsteroid gen1
    func :: Asteroid -> StdGen -> ([Asteroid], StdGen)
    func asteroid@(Asteroid path' _ (Rot r)) gen'
      | collided && ld >= 30 = ([a1,a2], gen2')
      | collided && ld < 30  = ([], gen')
      | otherwise            = ([Asteroid path' (move asteroid) (Rot r)], gen')
      where
        (a1, gen1') = splitAsteroid asteroid gen
        (a2, gen2') = splitAsteroid asteroid gen1'
        ld          = largestRadius path'
        collided    = any (isColliding asteroid) ps

isGameOver :: GameState -> Bool
isGameOver gs = hp1 <= 0 && (isSp || hp2 <= 0)
  where
    isSp = mode gs == Singleplayer
    hp1 = getHp $ playerOne gs
    hp2 = getHp $ playerTwo gs

updateWorld :: Float -> GameState -> GameState
updateWorld d gs
  | isGameOver gs = gs { screen = GameOver }
  | otherwise = newGs {
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
