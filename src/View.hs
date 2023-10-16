module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Model

import qualified Data.Set                         as S

width, height :: Int
width = 800
height = 600

window :: Display
window = InWindow "Asteroids" (width, height) (100, 100)

background :: Color
background = black

frames :: Int
frames = 30

render :: GameState -> IO Picture
render gs
  | currentScreen == Menu = renderMenu gs
  | currentScreen == InGame = renderInGame gs
  | otherwise = renderMenu gs
  where
    currentScreen = screen gs

renderText :: String -> Float -> Float -> Float -> Float -> Picture
renderText t x y sx sy = translate x y $ scale sx sy $ color white $ text t

centerPath :: [Point] -> [Point]
centerPath p = map (\(x, y) -> (x - xInc, y - yInc)) p
  where
    minX = minimum $ map fst p
    minY = minimum $ map snd p
    maxX = maximum $ map fst p
    maxY = maximum $ map snd p
    diffX = maxX - minX
    diffY = maxY - minY
    xInc = minX + (diffX / 2)
    yInc = minY + (diffY / 2)

renderSpaceShip :: Color -> Picture
renderSpaceShip c = scale 10 10 $ color c $ lineLoop $ centerPath [(0,0), (1,0),(1,-1), (2,-1), (2,-4), (3,-4), (3,-7), (2,-7), (2,-6), (-1,-6), (-1,-7), (-2,-7), (-2,-4), (-1,-4), (-1,-1), (0,-1), (0,0)]

renderMenu :: GameState -> IO Picture
renderMenu _ = return (
    Pictures [
      title,
      content,
      renderSpaceShip red
    ]
  )
  where
    title = renderText "Asteroids" (-125) 100 0.5 0.5
    content = renderText "Press A for singleplayer, D for multiplayer" (-275) 50 0.2 0.2

getRotation :: Player -> Float
getRotation (Player { rotation = Rot r }) = fromIntegral r

renderInGame :: GameState -> IO Picture
renderInGame gameState = return (
    Pictures [
      title,
      translate (x') (y') $ rotate (getRotation (playerOne gameState)) $ renderSpaceShip red,
      translate 50 50 $ rotate (getRotation (playerTwo gameState)) $ renderSpaceShip blue
      -- translate (-(fromIntegral width / 2 - 10)) (fromIntegral height / 2 - 20) $ scale 0.1 0.1 $ text (show $ selectedParticle gameState)
    ]
  )
  where
    title = renderText (show $ mode gameState) (-150) 100 0.5 0.5
    Pos Vector2 { x = x', y = y' } = position $ playerOne gameState



-- renderGameOver :: GameState -> IO Picture
