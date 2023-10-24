module Utils.Random where
import           Graphics.Gloss
import           System.Random

randomInt :: Int -> Int -> StdGen -> (Int, StdGen)
randomInt x y = randomR (x,y)

randomFloat :: Float -> Float -> StdGen -> (Float, StdGen)
randomFloat x y = randomR (x,y)

randomBool :: StdGen -> (Bool, StdGen)
randomBool gen
  | num == 0  = (False, gen1)
  | otherwise = (True, gen1)
  where
    (num, gen1) = randomInt 0 1 gen

mapRandom :: (a -> StdGen -> (b, StdGen)) -> [a] -> StdGen -> ([b], StdGen)
mapRandom _ [] gen     = ([], gen)
mapRandom f (x:xs) gen = (y:ys, gen1)
  where
    (y, gen1) = f x gen
    (ys, _)   = mapRandom f xs gen1

deviatePath :: Float -> Float -> Path -> StdGen -> (Path, StdGen)
deviatePath min' max' = mapRandom f
  where
    f (a,b) gen'
      | bool      = ((a - a', b - b'), gen3)
      | otherwise = ((a - (a' * 0.3), b - (b' * 0.3)), gen3)
      where
        (bool, gen1) = randomBool gen'
        (a', gen2)   = randomFloat min' max' gen1
        (b', gen3)   = randomFloat min' max' gen2
