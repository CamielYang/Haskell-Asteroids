module Model where

data Vector2  = Vector2 {
  x :: Int,
  y :: Int
}
newtype Position  = Pos Vector2
newtype Velocity  = Vel Vector2
newtype Rotation  = Rot Int
newtype Health    = HP Int
newtype Score     = Score Int

data Screen       = Menu | InGame | GameOver deriving (Eq)
data Status       = Paused | Active deriving (Eq)
data Mode         = Singleplayer | Multiplayer deriving (Eq)

data AsteroidType = AsteroidLg | AsteroidMd | AsteroidSm deriving (Eq, Ord)
data Asteroid     = Asteroid AsteroidType Position Velocity
data Projectile   = Projectile Position Velocity

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

data GameState = GameState {
  asteroids   :: [Asteroid],
  projectiles :: [Projectile],
  powerUps    :: [PowerUp],
  screen      :: Screen,
  mode        :: Mode,
  playerOne   :: Player,
  playerTwo   :: Player,
  score       :: Score,
  status      :: Status
}

initialPlayer :: Player
initialPlayer = Player {
  health   = HP 3,
  rotation = Rot 0,
  position = Pos Vector2 { x = 0, y = 0 },
  velocity = Vel Vector2 { x = 0, y = 0 },
  weapon   = Default
}

initialState :: GameState
initialState = GameState {
  asteroids = [],
  projectiles = [],
  powerUps = [],
  screen = Menu,
  mode = Singleplayer,
  playerOne = initialPlayer,
  playerTwo = initialPlayer,
  score = Score 0,
  status = Active
}
