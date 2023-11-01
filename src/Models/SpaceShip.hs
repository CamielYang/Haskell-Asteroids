module Models.SpaceShip where

import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Collidable
import           Models.Model
import           Models.Positioned
import           Utils.Keys
import           Utils.Lib

keyPressed :: GameState -> Key -> Bool
keyPressed gs k = S.member k (keys gs)

class SpaceShip a where
  getHp :: a -> Int
  getCooldown :: a -> Float
  isKilled :: a -> Bool
  collided :: a -> GameState -> Bool
  createProjectiles :: a -> GameState -> [Projectile]
  handleShoot :: a -> GameState -> [Projectile]
  updateCooldown :: a -> Float -> GameState -> Timer
  updateHealth :: a -> GameState -> Int
  updateRotation :: a -> GameState -> Rotation
  updateVelocity :: a -> GameState -> Velocity
  updatePlayer :: (a -> GameState) -> Float -> GameState -> a -> GameState

instance SpaceShip Player where
  getHp p = hp
    where
      HP hp _ = health p

  getCooldown (Player { health = HP _ (Time c) }) = c

  isKilled p = getHp p == 0

  collided p gs = any (isColliding p) (asteroids $ world gs) && getCooldown p <= 0

  createProjectiles p gs
    | isWt Shotgun = weaponShotgun (-10) : weaponShotgun 0 : weaponShotgun 10 : ps
    | isWt Rifle   = weaponRifle 1 : weaponRifle 2 : weaponRifle 3 : ps
    | otherwise    = weaponDefault : ps
    where
      isWt wt                = weapon p == wt
      ps                     = projectiles (world gs)
      dirVec                 = degreeToVector rot * Vec2 shootDistance shootDistance
      r@(Rot rot)            = rotation p
      create vec rot'        = Projectile (updatePosition p vec) rot' (Time projectileLifeTime)
      weaponDefault          = create dirVec r
      weaponShotgun deg      = create dirVec (Rot $ rot + deg)
      weaponRifle distanceMp = create (dirVec * distanceMp) r

  handleShoot p gs
    | keyPressed gs (shoot $ pKeys p) = createProjectiles p gs
    | otherwise                       = ps
    where
      ps    = projectiles (world gs)

  updateCooldown p delta gs
    | collided p gs     = Time 3
    | getCooldown p > 0 = Time $ getCooldown p - delta
    | otherwise         = Time 0

  updateHealth p gs
    | collided p gs && getCooldown p <= 0 = getHp p - 1
    | getHp p <= 0                        = 0
    | otherwise                           = getHp p

  updateRotation p gs
    | keyPressed gs (left $ pKeys p)  = setRotation p (-rotationSpeed)
    | keyPressed gs (right $ pKeys p) = setRotation p rotationSpeed
    | otherwise                       = rotation p
      where
        setRotation :: Player -> Int -> Rotation
        setRotation (Player { rotation = Rot r }) d
          | r + d > 360 = Rot (r + d - 360)
          | r + d < 0   = Rot (r + d + 360)
          | otherwise   = Rot (r + d)

  updateVelocity p gs
    | keyPressed gs (up $ pKeys p)   = setVelocity p force
    | keyPressed gs (down $ pKeys p) = setVelocity p (-force / 2)
    | otherwise                      = setVelocity p 0
      where
        setVelocity :: Player -> Float -> Velocity
        setVelocity p@(Player { velocity = Vel vVec }) v = if lengthOfVector newV2 > maxVelocity
          then velocity p
          else Vel newV2
          where
            Rot d   = rotation p
            dirVec  = degreeToVector d * Vec2 v v
            dragVec = Vec2 drag drag
            newV2   = (vVec + dirVec) * dragVec


  updatePlayer f dt gs p
    | isKilled p = gs
    | otherwise = (f newPlayer) {
      world = (world gs) {
        projectiles = handleShoot newPlayer gs
      }
    , keys = disableKeys (keys gs) [shoot $ pKeys p]
    }
    where
      newPlayer = p {
        rotation = updateRotation p gs
      , position = move p
      , velocity = updateVelocity p gs
      , health   = HP (updateHealth p gs) (updateCooldown p dt gs)
      }

