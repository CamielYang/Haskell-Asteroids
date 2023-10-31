module Models.Model where

import           Control.Monad
import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random

force,
  drag,
  maxVelocity,
  projectileSpeed,
  shootDistance,
  projectileLifeTime,
  boundMargin :: Float
rotationSpeed :: Int
force              = 0.3
drag               = 0.98
maxVelocity        = 10
projectileSpeed    = 10
rotationSpeed      = 10
shootDistance      = 25
projectileLifeTime = 2
boundMargin        = 35

data Vector2  = Vec2 {
  x :: Float,
  y :: Float
} deriving (Show)

instance Num Vector2 where
  (+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
  (-) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
  (*) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 * x2) (y1 * y2)
  abs (Vec2 x' y')              = Vec2 (abs x') (abs y')
  signum (Vec2 x' y')           = Vec2 (signum x') (signum y')
  fromInteger i                 = Vec2 (fromInteger i) (fromInteger i)

newtype Position  = Pos Vector2 deriving (Show)
newtype Velocity  = Vel Vector2
newtype Rotation  = Rot Int
newtype Health    = HP Int
newtype Timer     = Time Float
newtype State s a = S (s -> (a, s))

newtype Score     = Score Int deriving (Show)

data Screen       = Menu | InGame | GameOver | Pause deriving (Eq)
data Mode         = Singleplayer | Multiplayer deriving (Eq, Show)

data Asteroid     = Asteroid Path Position Rotation
data Projectile   = Projectile Position Rotation Timer

data WeaponType   = Default | Shotgun | Rifle deriving (Eq)
data PowerUpType  = Heart Int | Weapon WeaponType
data PowerUp      = PowerUp PowerUpType Position

data Player = Player {
  health   :: Health,
  rotation :: Rotation,
  position :: Position,
  pColor   :: Color,
  path     :: Path,
  velocity :: Velocity,
  weapon   :: WeaponType,
  cooldown :: Timer,
  pKeys    :: PlayerKeys
}

data PlayerKeys = PlayerKeys {
  up    :: Key,
  down  :: Key,
  left  :: Key,
  right :: Key,
  shoot :: Key
}

data World = World {
  asteroids   :: [Asteroid],
  projectiles :: [Projectile],
  powerUps    :: [PowerUp]
}

data GameState = GameState {
  world     :: World,
  screen    :: Screen,
  mode      :: Mode,
  playerOne :: Player,
  playerTwo :: Player,
  score     :: Score,
  keys      :: S.Set Key,
  stdGen    :: StdGen
}
