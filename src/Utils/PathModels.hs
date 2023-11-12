module Utils.PathModels (
  shipPath,
  asteroidPathScaled,
  asteroidPath,
  renderHeart,
  renderShotgun,
  renderRifle,
  renderDefault
) where
import           Graphics.Gloss
import           Models.Model
import           Utils.Lib
import           Utils.Point    (scalePath)
import           Utils.Random
import Data.Fixed (Pico)

centerPath :: Path -> Path
centerPath p = map (\(x', y') -> (x' - xInc, y' - yInc)) p
  where
    (minX, maxX) = minMax $ map fst p
    (minY, maxY) = minMax $ map snd p
    diffX = maxX - minX
    diffY = maxY - minY
    xInc  = minX + (diffX / 2)
    yInc  = minY + (diffY / 2)

shipPath :: Path
shipPath = scalePath 5 $ centerPath [(0,0), (1,0),(1,-1), (2,-1), (2,-4), (3,-4), (3,-7), (2,-7), (2,-6), (-1,-6), (-1,-7), (-2,-7), (-2,-4), (-1,-4), (-1,-1), (0,-1), (0,0)]

asteroidPathScaled :: Float -> Float -> GenState Path
asteroidPathScaled min' max' = do
  size <- randomFloat min' max'
  asteroidGen <- deviatePath 0.5 1 [(0,0), (1,0),(2,-1), (2,-2), (1,-3), (0,-3), (-1,-2), (-1,-1)]
  return $ scalePath size $ centerPath asteroidGen

asteroidPath :: GenState Path
asteroidPath = asteroidPathScaled 15 40

renderHeart :: Picture
renderHeart = lineLoop $ scalePath 10 $ centerPath [(0,0), (1,0), (1,-1), (2,-1), (2,-2), (1,-2), (1,-3), (0,-3), (0,-2), (-1,-2), (-1,-1), (0,-1)]

renderShotgun :: Picture
renderShotgun = lineLoop $ scalePath 10 $ centerPath [(0.2, 0), (1.8, 0), (2,-0.2), (2,-3.8), (2.2,-4), (-0.2,-4), (0,-3.8), (0,-0.2)]

renderRifle :: Picture
renderRifle = lineLoop $ scalePath 10 $ centerPath [(0.5,0), (0.8,-0.6), (0.8,-1.4), (1,-2), (1,-4), (0.8,-4.2), (1,-4.4), (0,-4.4), (0.2, -4.2), (0,-4), (0,-2), (0.2,-1.4), (0.2,-0.6)]

renderDefault :: Picture
renderDefault = lineLoop $ scalePath 10 $ centerPath [(0,0), (1,0), (2,-2), (2,-6), (-1,-6), (-1,-2)]