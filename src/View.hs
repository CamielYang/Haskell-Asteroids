module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Model

import qualified Data.Set                         as S

width, height :: Int
width = 800
height = 600

windowLeft, windowRight, windowTop, windowBottom :: Float
windowLeft = fromIntegral (-width) / 2
windowRight = fromIntegral width / 2
windowTop = fromIntegral height / 2
windowBottom = fromIntegral (-height) / 2

window :: Display
window = InWindow "Asteroids" (width, height) (100, 100)

background :: Color
background = makeColorI 36 28 65 255

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
renderSpaceShip c = scale 5 5 $ color c $ lineLoop $ centerPath [(0,0), (1,0),(1,-1), (2,-1), (2,-4), (3,-4), (3,-7), (2,-7), (2,-6), (-1,-6), (-1,-7), (-2,-7), (-2,-4), (-1,-4), (-1,-1), (0,-1), (0,0)]

renderMenu :: GameState -> IO Picture
renderMenu _ = return (
    Pictures [
      title,
      sp,
      mp
    ]
  )
  where
    title = renderText "Asteroids" (-125) 100 0.5 0.5
    sp = Pictures [
        renderText "Press A for singleplayer" (-150) 50 0.2 0.2,
        translate 0 0 $ renderSpaceShip red
      ]
    mp = Pictures [
        renderText "Press D for multiplayer" (-150) (-80) 0.2 0.2,
        translate (-25) (-130) $ Pictures [
          renderSpaceShip red,
          translate 50 0 $ renderSpaceShip yellow
        ]
      ]

getRotation :: Player -> Float
getRotation (Player { rotation = Rot r }) = fromIntegral r

getHp :: Player -> Int
getHp (Player { health = HP hp }) = hp

renderInGame :: GameState -> IO Picture
renderInGame gs = return (
    Pictures [
      renderPlayer p1 red,
      if isMp then renderPlayer p2 yellow else blank,
      renderProjectiles,
      title,
      renderScore,
      renderHpP1,
      renderHpP2
    ]
  )
  where
    title = renderText (show $ mode gs) (-75) (windowTop - 40) 0.2 0.2
    isMp = mode gs == Multiplayer
    p1 = playerOne gs
    p2 = playerTwo gs
    renderScore = renderText (show $ score gs) (-50) (windowBottom + 25) 0.2 0.2
    renderHpP1 = Pictures [translate (windowLeft + (25 * fromIntegral hp)) (windowTop - 25) $ scale 0.5 0.5 $ renderSpaceShip red | hp <- [1..getHp p1]]
    renderHpP2 = if isMp then Pictures [translate (windowRight - (25 * fromIntegral hp)) (windowTop - 25) $ scale 0.5 0.5 $ renderSpaceShip yellow | hp <- [1..getHp p2]] else blank
    renderProjectiles = Pictures $ map (\(Projectile (Pos Vector2 { x = x, y = y }) _) ->
      translate x y $ color white $ circleSolid 2) $ projectiles $ world gs
    renderPlayer p c = translate x y $ rotate (getRotation p) $ renderSpaceShip c
      where
        Pos (Vector2 x y) = position p

-- renderGameOver :: GameState -> IO Picture
