module Utils.Random where
import           Graphics.Gloss
import           Models.Model
import           Models.StateMonad
import           System.Random

randomInt :: Int -> Int -> State StdGen Int
randomInt x y = S $ \gen -> randomR (x,y) gen

randomFloat :: Float -> Float -> State StdGen Float
randomFloat x y = S $ \gen -> randomR (x,y) gen

randomBool :: State StdGen Bool
randomBool = do
  num <- randomInt 0 1
  return $ num /= 0

mapRandom :: (a -> State StdGen b) -> [a] -> State StdGen [b]
mapRandom _ []     = return []
mapRandom f (x:xs) = do
  y  <- f x
  ys <- mapRandom f xs
  return $ y:ys

deviatePath :: Float -> Float -> Path -> State StdGen Path
deviatePath min' max' = mapRandom f
  where
    f (a,b) = do
      bool <- randomBool
      a'   <- randomFloat min' max'
      b'   <- randomFloat min' max'
      return $ if bool
               then (a - a', b - b')
               else (a - (a' * 0.3), b - (b' * 0.3))
