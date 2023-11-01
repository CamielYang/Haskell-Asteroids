module Models.ModelLib where
import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Model
import           System.Random

initialPlayer :: Color -> PlayerKeys -> Player
initialPlayer c pks = Player {
  health   = HP 3 (Time 0),
  rotation = Rot 0,
  position = Pos Vec2 { x = 0, y = 0 },
  pColor   = c,
  velocity = Vel Vec2 { x = 0, y = 0 },
  weapon   = Shotgun,
  pKeys    = pks
}

initialWorld :: World
initialWorld = World {
  asteroids   = [],
  projectiles = [],
  powerUps    = []
}

p1Keys :: PlayerKeys
p1Keys = PlayerKeys {
  up    = Char 'w',
  down  = Char 's',
  left  = Char 'a',
  right = Char 'd',
  shoot = SpecialKey KeySpace
}

p2Keys :: PlayerKeys
p2Keys = PlayerKeys {
  up    = SpecialKey KeyUp,
  down  = SpecialKey KeyDown,
  left  = SpecialKey KeyLeft,
  right = SpecialKey KeyRight,
  shoot = SpecialKey KeyEnter
}

initialState :: GameState
initialState = GameState {
    world     = initialWorld,
    screen    = Menu,
    mode      = Singleplayer,
    playerOne = initialPlayer red p1Keys,
    playerTwo = (initialPlayer yellow p2Keys) { position = Pos Vec2 { x = 50, y = 0 }},
    score     = Score 0,
    keys     = S.empty,
    stdGen    = mkStdGen 100
  }

newGame :: GameState -> Mode -> GameState
newGame gs m = initialState { screen = InGame, mode = m, stdGen = stdGen gs }

addScore :: Score -> Score
addScore (Score s) = Score (s + 1)

getProjectiles :: GameState -> [Projectile]
getProjectiles gs = projectiles $ world gs

getAsteroids :: GameState -> [Asteroid]
getAsteroids gs = asteroids $ world gs

getPowerUps :: GameState -> [PowerUp]
getPowerUps gs = powerUps $ world gs
