module Model where

import qualified Data.Set                         as S
import           Graphics.Gloss.Interface.IO.Game
import           System.Random

force,
  drag,
  maxVelocity,
  projectileSpeed,
  shootDistance,
  projectileLifeTime :: Float
rotationSpeed :: Int
force              = 0.3
drag               = 0.98
maxVelocity        = 10
projectileSpeed    = 10
rotationSpeed      = 10
shootDistance      = 25
projectileLifeTime = 2

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

newtype Score     = Score Int deriving (Show)

data Screen       = Menu | InGame | GameOver deriving (Eq)
data Status       = Paused | Active deriving (Eq)
data Mode         = Singleplayer | Multiplayer deriving (Eq, Show)

data AsteroidType = AsteroidLg | AsteroidMd | AsteroidSm deriving (Eq, Ord)
data Asteroid     = Asteroid Path Position Rotation
data Projectile   = Projectile Position Rotation Timer

data WeaponType   = Default | Shotgun | Rifle
data PowerUpType  = Heart Int | Weapon WeaponType
data PowerUp      = PowerUp PowerUpType Position

data Player       = Player {
  health   :: Health,
  rotation :: Rotation,
  position :: Position,
  velocity :: Velocity,
  weapon   :: WeaponType,
  cooldown :: Timer
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
  status    :: Status,
  keys      :: S.Set Key,
  stdGen    :: StdGen
}

initialPlayer :: Player
initialPlayer = Player {
  health   = HP 3,
  rotation = Rot 0,
  position = Pos Vec2 { x = 0, y = 0 },
  velocity = Vel Vec2 { x = 0, y = 0 },
  weapon   = Default,
  cooldown = Time 0
}

initialWorld :: World
initialWorld = World {
  asteroids   = [],
  projectiles = [],
  powerUps    = []
}

initialState :: GameState
initialState = GameState {
  world     = initialWorld,
  screen    = Menu,
  mode      = Singleplayer,
  playerOne = initialPlayer,
  playerTwo = initialPlayer { position = Pos Vec2 { x = 50, y = 0 }},
  score     = Score 0,
  status    = Active,
  keys      = S.empty,
  stdGen    = mkStdGen 100
}

getRotation :: Player -> Int
getRotation (Player { rotation = Rot r }) = r

getHp :: Player -> Int
getHp (Player { health = HP hp }) = hp

addScore :: Score -> Score
addScore (Score s) = Score (s + 1)

getCooldown :: Player -> Float
getCooldown (Player { cooldown = Time c }) = c

determinePlayerColor :: Player -> Color -> Color
determinePlayerColor (Player { cooldown = Time cd }) c
  | cd <= 0    = c
  | otherwise  = withAlpha 0.5 c

getProjectiles :: GameState -> [Projectile]
getProjectiles gs = projectiles $ world gs

getAsteroids :: GameState -> [Asteroid]
getAsteroids gs = asteroids $ world gs

getPowerUps :: GameState -> [PowerUp]
getPowerUps gs = powerUps $ world gs

