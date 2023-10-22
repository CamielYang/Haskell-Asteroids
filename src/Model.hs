module Model where

import qualified Data.Set                         as S
import           Graphics.Gloss.Interface.IO.Game

data Vector2  = Vector2 {
  x :: Float,
  y :: Float
} deriving (Show)

newtype Position  = Pos Vector2 deriving (Show)
newtype Velocity  = Vel Vector2
newtype Rotation  = Rot Int
newtype Health    = HP Int

newtype Score     = Score Int deriving (Show)

data Screen       = Menu | InGame | GameOver deriving (Eq)
data Status       = Paused | Active deriving (Eq)
data Mode         = Singleplayer | Multiplayer deriving (Eq, Show)

data AsteroidType = AsteroidLg | AsteroidMd | AsteroidSm deriving (Eq, Ord)
data Asteroid     = Asteroid Path Position Rotation
data Projectile   = Projectile Position Rotation

data WeaponType   = Default | Shotgun | Rifle
data PowerUpType  = Heart Int | Weapon WeaponType
data PowerUp      = PowerUp PowerUpType Position


data Player       = Player {
  health   :: Health,
  rotation :: Rotation,
  position :: Position,
  velocity :: Velocity,
  weapon   :: WeaponType,
  cooldown :: Float
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
  keys      :: S.Set Key
}

initialPlayer :: Player
initialPlayer = Player {
  health   = HP 3,
  rotation = Rot 0,
  position = Pos Vector2 { x = 0, y = 0 },
  velocity = Vel Vector2 { x = 0, y = 0 },
  weapon   = Default,
  cooldown = 0
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
  playerTwo = initialPlayer { position = Pos Vector2 { x = 50, y = 0 }},
  score     = Score 0,
  status    = Active,
  keys      = S.empty
}

getRotation :: Player -> Int
getRotation (Player { rotation = Rot r }) = r

updateRotation :: Player -> Int -> Rotation
updateRotation (Player { rotation = Rot r }) d
  | r + d > 360 = Rot (r + d - 360)
  | r + d < 0   = Rot (r + d + 360)
  | otherwise   = Rot (r + d)

getHp :: Player -> Int
getHp (Player { health = HP hp }) = hp

addScore :: Score -> Score
addScore (Score s) = Score (s + 1)

getCooldown :: Player -> Float
getCooldown (Player { cooldown = c }) = c

determinePlayerColor :: Player -> Color -> Color
determinePlayerColor (Player { cooldown = cd }) c
  | cd <= 0    = c
  | otherwise  = withAlpha 0.5 c

getProjectiles :: GameState -> [Projectile]
getProjectiles gs = projectiles $ world gs

getAsteroids :: GameState -> [Asteroid]
getAsteroids gs = asteroids $ world gs

getPowerUps :: GameState -> [PowerUp]
getPowerUps gs = powerUps $ world gs

