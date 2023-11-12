module Models.Model where

import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random

width,
  height,
  force,
  drag,
  maxVelocity,
  projectileSpeed,
  shootDistance,
  projectileLifeTime,
  particleLifeTime,
  boundMargin,
  minAsteroidSize :: Float
rotationSpeed :: Int
width = 1500
height = 750

force              = 0.3  -- Amount of velocity you can build each frame (pixels).
drag               = 0.98 -- Amount of speed that is lost each frame (fraction)
maxVelocity        = 10   -- Max amount of speed the ship can travel (pixel).
projectileSpeed    = 12   -- Max amount of speed projectile can travel (pixels).
rotationSpeed      = 10   -- Amount of degrees which can be added each frame.
shootDistance      = 25   -- Starting point distance from center of spaceship.
projectileLifeTime = 2    -- Time projectile can live.
particleLifeTime   = 0.5    -- Time particle can live.
boundMargin        = 35   -- Margin between screen edge before teleporting back into screen.
minAsteroidSize    = 30   -- Minimum size of asteroid.

newtype State s a = S (s -> (a, s))
type GenState a   = State StdGen a

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

newtype Updated   = Updated Bool deriving (Read)
newtype Position  = Pos Vector2 deriving (Show)
newtype Velocity  = Vel Vector2
newtype Rotation  = Rot Int
newtype Timer     = Time Float
newtype Score     = Score Int deriving (Show)

data Screen       = Menu | InGame | GameOver | Pause deriving (Eq)
data Mode         = Singleplayer | Multiplayer deriving (Eq, Show)
data Highscore    = Highscore Int Updated deriving (Read)

data Asteroid     = Asteroid Path Position Rotation
data Projectile   = Projectile Position Rotation Timer
data Particle     = Particle Asteroid Timer

data WeaponType   = Default | Shotgun | Rifle deriving (Eq)
data PowerUpType  = Heart Int | Weapon WeaponType
data PowerUp      = PowerUp PowerUpType Position
data Health       = HP Int Timer

data Player = Player {
  health   :: Health,
  rotation :: Rotation,
  position :: Position,
  pColor   :: Color,
  velocity :: Velocity,
  weapon   :: WeaponType,
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
  powerUps    :: [PowerUp],
  particles   :: [Particle]
}

data GameState = GameState {
  world          :: World,
  screen         :: Screen,
  mode           :: Mode,
  playerOne      :: Player,
  playerTwo      :: Player,
  score          :: Score,
  keys           :: S.Set Key,
  powerUpSpawned :: Updated,
  stdGen         :: StdGen,
  highscore      :: Highscore
}
