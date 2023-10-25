module Models.SpaceShip where

import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Collidable
import           Models.Model
import           Models.Positioned
import           Utils.Lib

keyPressed :: GameState -> Key -> Bool
keyPressed gs k = S.member k (keys gs)

class SpaceShip a where
  getHp :: a -> Int
  getCooldown :: a -> Float
  isKilled :: a -> Bool
  createProjectiles :: a -> GameState -> [Projectile]
  handleShoot :: a -> GameState -> [Projectile]
  updateCooldown :: a -> Float -> GameState -> Timer
  updateHealth :: a -> GameState -> Health
  updateRotation :: a -> GameState -> Rotation
  updateVelocity :: a -> GameState -> Velocity
  updatePlayer :: (a -> GameState) -> Float -> GameState -> a -> GameState

instance SpaceShip Player where
  getHp p = hp
    where
      HP hp = health p

  getCooldown (Player { cooldown = Time c }) = c

  isKilled p = getHp p == 0

  createProjectiles p gs
    | isWt Shotgun = weaponShotgun (-10) : weaponShotgun 0 : weaponShotgun 10 : ps
    | isWt Rifle   = weaponRifle 1 : weaponRifle 2 : weaponRifle 3 : ps
    | otherwise     = weaponDefault : ps
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
    | otherwise  = ps
    where
      ps    = projectiles (world gs)

  updateCooldown p delta gs
    | shipCollided p gs = Time 3
    | getCooldown p > 0 = Time $ getCooldown p - delta
    | otherwise         = Time 0

  updateHealth p gs
    | shipCollided p gs && getCooldown p <= 0 = HP (getHp p - 1)
    | getHp p <= 0                            = HP 0
    | otherwise                               = health p

  updateRotation p gs
    | keyPressed gs (left $ pKeys p) = setRotation p (-rotationSpeed)
    | keyPressed gs (right $ pKeys p) = setRotation p rotationSpeed
    | otherwise            = rotation p

  updateVelocity p gs
    | keyPressed gs (up $ pKeys p) = setVelocity p force
    | keyPressed gs (down $ pKeys p) = setVelocity p (-force / 2)
    | otherwise            = setVelocity p 0

  updatePlayer f dt gs p
    | isKilled p = gs
    | otherwise = (f newPlayer) {
      world = (world gs) {
        projectiles = handleShoot newPlayer gs
      }
    }
    where
      newPlayer = p {
        rotation = updateRotation p gs
      , position = move p
      , velocity = updateVelocity p gs
      , health   = updateHealth p gs
      , cooldown = updateCooldown p dt gs
      }

