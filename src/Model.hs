module Model where

import qualified Data.Set                         as S
import           Graphics.Gloss.Interface.IO.Game

data Vector2  = Vector2 {
  x :: Float,
  y :: Float
}

newtype Position  = Pos Vector2
newtype Velocity  = Vel Vector2
newtype Rotation  = Rot Int
newtype Health    = HP Int
newtype Score     = Score Int deriving (Show)

data Screen       = Menu | InGame | GameOver deriving (Eq)
data Status       = Paused | Active deriving (Eq)
data Mode         = Singleplayer | Multiplayer deriving (Eq, Show)

data AsteroidType = AsteroidLg | AsteroidMd | AsteroidSm deriving (Eq, Ord)
data Asteroid     = Asteroid AsteroidType Picture Position Velocity
data Projectile   = Projectile Position Rotation

data WeaponType   = Default | Shotgun | Rifle
data PowerUpType  = Heart Int | Weapon WeaponType
data PowerUp      = PowerUp PowerUpType Position


data Player       = Player {
  health   :: Health,
  rotation :: Rotation,
  position :: Position,
  velocity :: Velocity,
  weapon   :: WeaponType
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
  weapon   = Default
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
