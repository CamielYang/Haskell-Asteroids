module Utils.Random where
import           Graphics.Gloss
import           Models.Model
import           Models.Monad
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

randomInt' :: Int -> Int -> State StdGen Int
randomInt' x y = S $ \gen -> randomR (x,y) gen

randomFloat' :: Float -> Float -> State StdGen Float
randomFloat' x y = S $ \gen -> randomR (x,y) gen

randomBool' :: State StdGen Bool
randomBool' = do
  num <- randomInt' 0 1
  return $ num /= 0

mapRandom' :: (a -> State StdGen b) -> [a] -> State StdGen [b]
mapRandom' _ []     = return []
mapRandom' f (x:xs) = do
  y  <- f x
  ys <- mapRandom' f xs
  return $ y:ys

deviatePath' :: Float -> Float -> Path -> State StdGen Path
deviatePath' min' max' = mapRandom' f
  where
    f (a,b) = do
      bool <- randomBool'
      a'   <- randomFloat' min' max'
      b'   <- randomFloat' min' max'
      return $ if bool
               then (a - a', b - b')
               else (a - (a' * 0.3), b - (b' * 0.3))
