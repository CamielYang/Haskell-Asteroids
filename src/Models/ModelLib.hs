module Models.ModelLib where
import qualified Data.Set         as S
import           Graphics.Gloss
import           Models.Model
import           System.Random
import           Utils.PathModels

initialPlayer :: Color -> Player
initialPlayer c = Player {
  health   = HP 3,
  rotation = Rot 0,
  position = Pos Vec2 { x = 0, y = 0 },
  pColor    = c,
  path     = shipPath,
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
    playerOne = initialPlayer red,
    playerTwo = (initialPlayer yellow) { position = Pos Vec2 { x = 50, y = 0 }},
    score     = Score 0,
    keys      = S.empty,
    stdGen    = mkStdGen 100
  }

newGame :: GameState -> Mode -> GameState
newGame gs m = initialState { screen = InGame, mode = m, stdGen = stdGen gs }

getHp :: Player -> Int
getHp (Player { health = HP hp }) = hp

addScore :: Score -> Score
addScore (Score s) = Score (s + 1)

getCooldown :: Player -> Float
getCooldown (Player { cooldown = Time c }) = c

getProjectiles :: GameState -> [Projectile]
getProjectiles gs = projectiles $ world gs

getAsteroids :: GameState -> [Asteroid]
getAsteroids gs = asteroids $ world gs

getPowerUps :: GameState -> [PowerUp]
getPowerUps gs = powerUps $ world gs

isKilled :: Player -> Bool
isKilled p = getHp p == 0

