module Controllers.Game (updateGame, gameKeys) where
import qualified Data.Set                           as S
import           Graphics.Gloss.Interface.Pure.Game
import           Models.Collidable
import           Models.Model
import           Models.ModelLib
import           Models.Positioned
import           Models.SpaceShip
import           Models.StateMonad
import           Utils.PathModels
import           Utils.Point                        (scalePath)
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

randomPowerUp :: GenState PowerUpType
randomPowerUp = do
  choice <- randomInt 0 3
  let result | choice == 0 = Heart 1
             | choice == 1 = Weapon Default
             | choice == 2 = Weapon Shotgun
             | choice == 3 = Weapon Rifle
  return result

updatePowerUp :: GameState -> GenState [PowerUp]
updatePowerUp = undefined

updateAsteroids :: GameState -> GenState [Asteroid]
updateAsteroids gs@(GameState { world = World { asteroids = as, projectiles = ps } }) = do
  collidedAs    <- foldRandom updateCollided [] as

  let asteroidCount = round $ 10 + (1 / 10) * fromIntegral (getScore gs) -- Increasing amount of asteroids over time.

  let result | length as < asteroidCount = do
                 spawnAsteroid <- randomAsteroid gs
                 return $ spawnAsteroid : collidedAs
             | otherwise                 = return collidedAs
      in result
  where
    updateCollided a asteroid@(Asteroid path' _ (Rot r)) = do
      let result | psCollided && lr >= 30 = do
                     a1 <- splitAsteroid asteroid
                     a2 <- splitAsteroid asteroid
                     return $ a1 : a2 :a
                 | psCollided && lr < 30  = return a
                 | otherwise              = return $ Asteroid path' (move asteroid) (Rot r) : a
          in result
      where
        lr         = largestRadius path'
        psCollided = any (isColliding asteroid) ps

createParticle :: Asteroid -> GenState Particle
createParticle asteroid = do
  a@(Asteroid p _ r)   <- splitAsteroid asteroid
  return $ Particle (Asteroid p (move a) r) (Time particleLifeTime)

createParticles :: Asteroid -> [Particle] -> GenState [Particle]
createParticles asteroid ps = do
  add <- randomBool

  if (add && length ps < 5) || length ps < 2
  then do
    p1 <- createParticle asteroid
    createParticles asteroid (p1 : ps)
  else do
    return ps

updateParticles :: Float -> GameState -> GenState [Particle]
updateParticles d (GameState { world = World { asteroids = as, projectiles = ps, particles = prts } }) = do
  collidedAs <- foldRandom updateCollided [] as

  let newPrts = foldr updateParticles' [] prts

  return $ collidedAs ++ newPrts
  where
    updateParticles' p@(Particle (Asteroid path' _ (Rot r)) (Time t)) b
      | t > 0     = Particle (Asteroid path' (move p) (Rot r)) (Time $ t - d) : b
      | otherwise = b
    updateCollided ps' asteroid@(Asteroid path' _ _) = do
      let result | psCollided && lr < minAsteroidSize = do
                     createParticles asteroid []
                 | otherwise = return ps'
          in result
      where
        lr         = largestRadius path'
        psCollided = any (isColliding asteroid) ps

randomAsteroid :: GameState -> GenState Asteroid
randomAsteroid gs = do
  path' <- asteroidPath
  rot   <- randomInt 0 360
  x'    <- randomFloat windowLeft windowRight
  y'    <- randomFloat windowBottom windowTop

  let pos' = Pos (Vec2 x' y')
  let rot' = Rot rot
  let asteroid = Asteroid path' pos' rot'
  let asteroidScaled = Asteroid (scalePath 1.5 path') pos' rot'

  if isColliding asteroidScaled (playerOne gs) ||
    (mode gs == Multiplayer && isColliding asteroidScaled (playerTwo gs))
  then randomAsteroid gs
  else return asteroid

splitAsteroid :: Asteroid -> GenState Asteroid
splitAsteroid a = do
  dX    <- randomFloat 10 20
  dY    <- randomFloat 10 20
  dR    <- randomInt 0 360
  aPath <- asteroidPathScaled size size
  return $ Asteroid aPath (setPosition a (Vec2 dX dY)) (Rot dR)
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
                        powerUps  = [],
                        particles = prts
                      },
                      stdGen = gen
                    }
  where
    newUp     = runState (updatePowerUp gs) (stdGen gs)
    (as, gen) = runState (updateAsteroids gs) (stdGen gs)
    newGs     = updateProjectiles d gs
    (prts, _) = runState (updateParticles d gs) (stdGen gs)

updateGame :: Float -> GameState -> GameState
updateGame d gs = gs2
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
