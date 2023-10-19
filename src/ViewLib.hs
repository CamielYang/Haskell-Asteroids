module ViewLib where
import           Graphics.Gloss
import           Lib
import           Model

width, height :: Int
width = 800
height = 600

windowLeft, windowRight, windowTop, windowBottom :: Float
windowLeft = fromIntegral (-width) / 2
windowRight = fromIntegral width / 2
windowTop = fromIntegral height / 2
windowBottom = fromIntegral (-height) / 2

renderText :: String -> Float -> Float -> Float -> Float -> Picture
renderText t x' y' sx sy = translate x' y' $ scale sx sy $ color white $ text t

centerPath :: [Point] -> [Point]
centerPath p = map (\(x', y') -> (x' - xInc, y' - yInc)) p
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

randomizeCoords :: [Point] -> IO [Point]
randomizeCoords = mapM f
  where
    f (a,b) = do
      bool <- randomBool
      a' <- drawFloat 0.5 1
      b' <- drawFloat 0.5 1
      return $ if bool then (a - a', b - b') else (a - (a' / 3), b - (b' / 3))

renderAsteroid :: AsteroidType -> IO Picture
renderAsteroid at = do
  randomizedPath <- randomizeCoords [(0,0), (1,0),(2,-1), (2,-2), (1,-3), (0,-3), (-1,-2), (-1,-1)]
  return $ sc $ color white $ lineLoop $ centerPath randomizedPath
  where
    sc
      | at == AsteroidSm = scale 15 15
      | at == AsteroidMd = scale 25 25
      | otherwise = scale 40 40

getRotation :: Player -> Float
getRotation (Player { rotation = Rot r }) = fromIntegral r

getHp :: Player -> Int
getHp (Player { health = HP hp }) = hp
