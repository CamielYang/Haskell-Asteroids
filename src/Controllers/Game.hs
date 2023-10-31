module Controllers.Game (updateGame, gameKeys) where
import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Collidable
import           Models.Model
import           Models.ModelLib
import           Models.Positioned
import           Models.SpaceShip
import           Models.StateMonad
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

updateAsteroids :: GameState -> GenState [Asteroid]
updateAsteroids (GameState { world = World { asteroids = as, projectiles = ps } }) = do
  collidedAs    <- mapRandom updateCollided as
  spawnAsteroid <- randomAsteroid

  let collidedAs' = concat collidedAs
  let result | length as < 10 = spawnAsteroid : collidedAs'
             | otherwise      = collidedAs'

  return result
  where
    updateCollided :: Asteroid -> GenState [Asteroid]
    updateCollided asteroid@(Asteroid path' _ (Rot r)) = do
      a1 <- splitAsteroid asteroid
      a2 <- splitAsteroid asteroid

      let result | psCollided && ld >= 30 = [a1,a2]
                 | psCollided && ld < 30  = []
                 | otherwise              = [Asteroid path' (move asteroid) (Rot r)]

      return result
      where
        ld         = largestRadius path'
        psCollided = any (isColliding asteroid) ps

randomAsteroid :: GenState Asteroid
randomAsteroid = do
  path' <- asteroidPath
  rot   <- randomInt 0 360
  x'    <- randomFloat windowLeft windowRight
  y'    <- randomFloat windowBottom windowTop
  return $ Asteroid path' (Pos (Vec2 x' y')) (Rot rot)

splitAsteroid :: Asteroid -> GenState Asteroid
splitAsteroid a = do
  dX    <- randomFloat 0 10
  dY    <- randomFloat 0 10
  dR    <- randomInt 0 360
  aPath <- asteroidPathScaled size size
  return $ Asteroid aPath (updatePosition a (Vec2 dX dY)) (Rot dR)
  where
    size = getHitboxRadius a / 3

isGameOver :: GameState -> Bool
isGameOver gs = isKilled (playerOne gs) && (isSp || isKilled (playerTwo gs))
  where
    isSp = mode gs == Singleplayer

updateWorld :: Float -> GameState -> GameState
updateWorld d gs
  | isGameOver gs = gs { screen = GameOver }
  | otherwise     = newGs {
                      world = (world newGs) {
                        asteroids = as,
                        powerUps  = []
                      },
                      stdGen = gen
                    }
  where
    (as, gen) = runState (updateAsteroids gs) (stdGen gs)
    newGs     = updateProjectiles d gs

-- obtainPowerUp :: PowerUpType -> Player -> Player
-- obtainPowerUp (Heart n) player = player { health = HP (n + getHealth (health player)) }
--   where
--     getHealth :: Health -> Int
--     getHealth (HP v) = v
-- obtainPowerUp (Weapon weaponType) player = player { weapon = weaponType }

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
