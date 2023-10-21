module Utils.PathModels (
  shipPath,
  asteroidPath
) where
import           Graphics.Gloss
import           Utils.Point    (scalePath)
import           Utils.Random

centerPath :: Path -> Path
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

shipPath :: Path
shipPath = scalePath 5 $ centerPath [(0,0), (1,0),(1,-1), (2,-1), (2,-4), (3,-4), (3,-7), (2,-7), (2,-6), (-1,-6), (-1,-7), (-2,-7), (-2,-4), (-1,-4), (-1,-1), (0,-1), (0,0)]

asteroidPath :: IO Path
asteroidPath = do
  size <- randomFloat 15 40
  asteroidGen <- deviatePath 0.5 1 [(0,0), (1,0),(2,-1), (2,-2), (1,-3), (0,-3), (-1,-2), (-1,-1)]
  return $ scalePath size $ centerPath asteroidGen