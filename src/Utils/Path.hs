module Utils.Path (shipPath, asteroidPath) where
import           Graphics.Gloss
import           Model
import           Model          (AsteroidType)
import           Utils.Lib
import           Utils.Point    (scalePath)

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

shipPath :: [Point]
shipPath = scalePath 5 $ centerPath [(0,0), (1,0),(1,-1), (2,-1), (2,-4), (3,-4), (3,-7), (2,-7), (2,-6), (-1,-6), (-1,-7), (-2,-7), (-2,-4), (-1,-4), (-1,-1), (0,-1), (0,0)]

asteroidPath :: AsteroidType -> IO [Point]
asteroidPath at = do
  asteroidGen <- randomizeCoords [(0,0), (1,0),(2,-1), (2,-2), (1,-3), (0,-3), (-1,-2), (-1,-1)]
  return $ sc $ centerPath asteroidGen
  where
    sc
      | at == AsteroidSm = scalePath 15
      | at == AsteroidMd = scalePath 25
      | otherwise = scalePath 40

