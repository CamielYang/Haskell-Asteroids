module Controller where

import qualified Data.Set                         as S
import           Graphics.Gloss.Interface.IO.Game
import           Model

updateRotation :: Player -> Int -> Rotation
updateRotation (Player { rotation = Rot r }) delta = Rot (r + delta)

updatePosition :: Player -> Int -> Position
updatePosition p@(Player { position = Pos (Vector2 x y) }) delta = Pos (Vector2 (x + xInc) (y + yInc))
  where
    xInc = cos radians * fromIntegral delta
    yInc = sin radians * fromIntegral delta
    Rot d = rotation p
    radians :: Float
    radians = fromIntegral (-d + 90) * pi / 180

update :: Float -> GameState -> IO GameState
update _ gameState = do
  playerOne <- do
    return $ (playerOne gameState) {
      rotation = if S.member (Char 'a') (keys gameState)
        then updateRotation (playerOne gameState) (-10)
        else if S.member (Char 'd') (keys gameState)
          then updateRotation (playerOne gameState) 10
          else rotation (playerOne gameState)
    , position = if S.member (Char 'w') (keys gameState)
        then updatePosition (playerOne gameState) 15
        else if S.member (Char 's') (keys gameState)
          then updatePosition (playerOne gameState) (-15)
          else position (playerOne gameState)
    }
  playerTwo <- do
    return $ (playerTwo gameState) {
      rotation = if S.member (SpecialKey KeyLeft) (keys gameState)
        then updateRotation (playerTwo gameState) (-10)
        else if S.member (SpecialKey KeyRight) (keys gameState)
          then updateRotation (playerTwo gameState) 10
          else rotation (playerTwo gameState)
    }

  return gameState {
    playerOne = playerOne
  , playerTwo = playerTwo
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

