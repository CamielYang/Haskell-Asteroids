module Controllers.Game (updateGame, gameKeys) where
import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Collidable
import           Models.Model
import           Models.ModelLib
import           Models.Positioned
import           Models.SpaceShip
import           System.Random                      (StdGen)
import           Utils.Keys
import           Utils.PathModels
import           Utils.Random
import           Utils.Render

updateProjectiles :: Float -> GameState -> GameState
updateProjectiles _ gs@(GameState { world = World { projectiles = [] } }) = gs
updateProjectiles d gs@(GameState { world = World { projectiles = ps, asteroids = as } }) =
  gs {
    world = (world gs) { projectiles = map func filtered },
    score = if (not . all notCollided) ps
            then addScore (score gs)
            else score gs
  }
  where
    filtered                               = filter (\p -> notAged p && notCollided p) ps
    notCollided p                          = not (any (isColliding p) as)
    notAged (Projectile _ _ (Time t))      = t > 0
    func p@(Projectile _ (Rot r) (Time t)) = Projectile (move p) (Rot r) (Time $ t - d)

randomPowerUp :: StdGen -> GenState
randomPowerUp gen
  | choice == 0 = Heart 10, gen'
  | choice == 1 = (Weapon Default, gen')
  | choice == 2 = (Weapon Shotgun, gen')
  | choice == 3 = (Weapon Rifle, gen')
  where
    (choice, gen') = randomInt 0 3 gen

randomAsteroid :: StdGen -> (Asteroid, StdGen)
randomAsteroid gen = (Asteroid path' (Pos (Vec2 x' y')) (Rot rot), gen4)
  where
    (path', gen1) = asteroidPath gen
    (rot, gen2)   = randomInt 0 360 gen1
    (x', gen3)    = randomFloat windowLeft windowRight gen2
    (y', gen4)    = randomFloat windowBottom windowTop gen3

splitAsteroid :: Asteroid -> StdGen -> (Asteroid, StdGen)
splitAsteroid a gen = (Asteroid path' (updatePosition a (Vec2 dX dY)) (Rot dR), gen4)
  where
    (dX, gen1)    = randomFloat 0 10 gen
    (dY, gen2)    = randomFloat 0 10 gen1
    (dR, gen3)    = randomInt 0 360 gen2
    (path', gen4) = asteroidPathScaled size size gen3
    size          = getHitboxRadius a / 3

updatePowerups :: GameState -> stdGen -> ([PowerUp], StdGen)
updatePowerups = 

updateAsteroids :: GameState -> StdGen -> ([Asteroid], StdGen)
updateAsteroids (GameState { world = World { asteroids = as, projectiles = ps } }) gen
  | length as < 10 = (newAsteroid : newAs, gen2)
  | otherwise      = (newAs, gen2)
  where
    (mapped, gen1)      = mapRandom func as gen
    newAs               = concat mapped
    (newAsteroid, gen2) = randomAsteroid gen1
    func :: Asteroid -> StdGen -> ([Asteroid], StdGen)
    func asteroid@(Asteroid path' _ (Rot r)) gen'
      | psCollided && ld >= 30 = ([a1,a2], gen2')
      | psCollided && ld < 30  = ([], gen')
      | otherwise            = ([Asteroid path' (move asteroid) (Rot r)], gen')
      where
        (a1, gen1') = splitAsteroid asteroid gen
        (a2, gen2') = splitAsteroid asteroid gen1'
        ld          = largestRadius path'
        psCollided    = any (isColliding asteroid) ps

isGameOver :: GameState -> Bool
isGameOver gs = isKilled (playerOne gs) && (isSp || isKilled (playerTwo gs))
  where
    isSp = mode gs == Singleplayer

updateWorld :: Float -> GameState -> GameState
updateWorld d gs
  | isGameOver gs = gs { screen = GameOver }
  | otherwise = newGs {
    world = (world newGs) {
      asteroids = newAs,
      powerUps  = []
    },
    stdGen = newGen
  }
  where
    newPu = updatePowerups -- etc
    (newAs, newGen) = updateAsteroids gs (stdGen gs)
    newGs           = updateProjectiles d gs

obtainPowerUp :: PowerUpType -> Player -> Player
obtainPowerUp (Heart n) player = player { health = HP (n + getHealth (health player)) }
  where
    getHealth :: Health -> Int
    getHealth (HP v) = v
obtainPowerUp (Weapon weaponType) player = player { weapon = weaponType }

updateGame :: Float -> GameState -> GameState
updateGame d gs = gs2 {
    keys = disableKeys (keys gs2) [SpecialKey KeyEnter, SpecialKey KeySpace]
  }
  where
    newGs = updateWorld d gs
    gs1 = updatePlayer
            (\p -> newGs { playerOne = p })
            d
            newGs
            (playerOne newGs)
    gs2 = if mode gs == Multiplayer
          then
          updatePlayer
            (\p -> gs1 { playerTwo = p })
            d
            gs1
            (playerTwo gs1)
          else gs1

gameKeys :: Event -> GameState -> GameState
gameKeys (EventKey (SpecialKey KeyEsc) Down _ _) gs = gs { screen = Pause }
gameKeys (EventKey k Down _ _) gs                   = gs { keys = S.insert k (keys gs)}
gameKeys (EventKey k Up _ _) gs                     = gs { keys = S.delete k (keys gs)}
gameKeys _ gs                                       = gs
