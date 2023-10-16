module Controller where

import           Data.Char                        (GeneralCategory (Space))
import qualified Data.Set                         as S
import           Graphics.Gloss.Interface.IO.Game
import           Model

updateRotation :: Player -> Int -> Rotation
updateRotation (Player { rotation = Rot r }) delta = Rot (r + delta)

degreeToVector :: Int -> Vector2
degreeToVector d = Vector2 (cos radians) (sin radians)
  where
    radians :: Float
    radians = fromIntegral (-d + 90) * pi / 180

updatePosition :: Player -> Float -> Position
updatePosition p@(Player { position = Pos (Vector2 x y) }) v = Pos (Vector2 (x + xInc * v) (y + yInc * v))
  where
    Rot d = rotation p
    Vector2 { x = xInc, y = yInc } = degreeToVector d

updatePlayer :: GameState -> Player -> Key -> Key -> Key -> Key -> Key -> (Player, [Projectile])
updatePlayer gs p@(Player { position = Pos (Vector2 x y), rotation = Rot rot }) u d l r s = do
  (p {
      rotation = if S.member l (keys gs)
        then updateRotation p (-10)
        else if S.member r (keys gs)
          then updateRotation p 10
          else rotation p
    , position = if S.member u (keys gs)
        then updatePosition p 15
        else if S.member d (keys gs)
          then updatePosition p (-15)
          else position p
    },
    if S.member s (keys gs)
        then
          Projectile (Pos (Vector2 (x + xInc * 40) (y + yInc * 40))) (Rot rot) : ps
        else
          ps)
    where
      Vector2 { x = xInc, y = yInc } = degreeToVector rot
      ps = projectiles (world gs)

updateProjectiles :: GameState -> GameState
updateProjectiles gs@(GameState { world = World { projectiles = [] } }) = gs
updateProjectiles gs@(GameState { world = World { projectiles = ps } }) = gs {
    world = (world gs) { projectiles = map func ps }
  }
  where
    func (Projectile (Pos (Vector2 x y)) (Rot r)) = Projectile (Pos (Vector2 (x + xInc * 10) (y + yInc * 10))) (Rot r)
      where
        Vector2 { x = xInc, y = yInc } = degreeToVector r

disableKeys :: S.Set Key -> [Key] -> S.Set Key
disableKeys s [] = s
disableKeys s (k:ks)
  | S.member k s = disableKeys (S.delete k s) ks
  | otherwise = disableKeys s ks

update :: Float -> GameState -> IO GameState
update _ gs = do
  let newGs = updateProjectiles gs

  let gs1 = (\(p1, ps) -> newGs {
                playerOne = p1
              , world = (world newGs) { projectiles = ps }
            }) (updatePlayer
                  newGs
                  (playerOne newGs)
                  (Char 'w')
                  (Char 's')
                  (Char 'a')
                  (Char 'd')
                  (SpecialKey KeySpace))

  let gs2 = (\(p, ps) -> gs1 {
              playerTwo = p
            , world = (world gs1) { projectiles = ps }
          }) (updatePlayer
                gs1
                (playerTwo gs1)
                (SpecialKey KeyUp)
                (SpecialKey KeyDown)
                (SpecialKey KeyLeft)
                (SpecialKey KeyRight)
                (SpecialKey KeyEnter))

  return gs2 {
    keys = disableKeys (keys gs2) [SpecialKey KeyEnter, SpecialKey KeySpace]
  }

handleKeys :: Event -> GameState -> IO GameState
handleKeys e gameState
  | currentScreen == Menu = return (gameMenuKeys e gameState)
  | currentScreen == InGame = return (inGameKeys e gameState)
  | otherwise = return (gameOverKeys e gameState)
  where
    currentScreen = screen gameState

gameMenuKeys :: Event -> GameState -> GameState
gameMenuKeys (EventKey (Char 'a') Down _ _) gameState = gameState { screen = InGame, mode = Singleplayer }
gameMenuKeys (EventKey (Char 'd') Down _ _) gameState = gameState { screen = InGame, mode = Multiplayer }
gameMenuKeys _ gameState = gameState

inGameKeys :: Event -> GameState -> GameState
inGameKeys (EventKey (SpecialKey KeyEsc) Down _ _) gameState =
  gameState { status = toggleStatus (status gameState) }
  where
    toggleStatus Active = Paused
    toggleStatus Paused = Active
inGameKeys (EventKey k Down _ _) world = world { keys = S.insert k (keys world)}
inGameKeys (EventKey k Up _ _) world = world { keys = S.delete k (keys world)}
inGameKeys _ gameState = gameState

gameOverKeys :: Event -> GameState -> GameState
gameOverKeys _ gameState = gameState

obtainPowerUp :: PowerUpType -> Player -> Player
obtainPowerUp (Heart n) player = player { health = HP (n + getHealth (health player)) }
  where
    getHealth :: Health -> Int
    getHealth (HP v) = v
obtainPowerUp (Weapon weaponType) player = player { weapon = weaponType }

