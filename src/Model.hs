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

data Status       = Paused | Active | GameOver deriving (Eq)

data AsteroidType = AsteroidLg | AsteroidMd | AsteroidSm deriving (Eq, Ord)
data Asteroid     = Asteroid AsteroidType Position Velocity
data Projectile   = Projectile Position Velocity
data Player       = Player {
  health   :: Health,
  rotation :: Rotation,
  position :: Position,
  velocity :: Velocity
}

data GameState = GameState {
  asteroids   :: [Asteroid],
  projectiles :: [Projectile],
  player      :: Player,
  score       :: Score,
  status      :: Status
}

initialState :: GameState
initialState = GameState {
  asteroids = [],
  projectiles = [],
  player = Player {
    health   = HP 3,
    rotation = Rot 0,
    position = Pos Vector2 { x = 0, y = 0 },
    velocity = Vel Vector2 { x = 0, y = 0 }
  },
  score = Score 0,
  status = Active
}
