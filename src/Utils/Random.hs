module Utils.Random where
import           Graphics.Gloss
import           Models.Model
import           Models.StateMonad ()
import           System.Random

randomInt :: Int -> Int -> GenState Int
randomInt x' y' = S $ \gen -> randomR (x', y') gen

randomFloat :: Float -> Float -> GenState Float
randomFloat x' y' = S $ \gen -> randomR (x', y') gen

randomBool :: GenState Bool
randomBool = do
  num <- randomInt 0 1
  return $ num /= 0

mapRandom :: (a -> GenState b) -> [a] -> GenState [b]
mapRandom _ []     = return []
mapRandom f (x:xs) = do
  y  <- f x
  ys <- mapRandom f xs
  return $ y:ys

deviatePath :: Float -> Float -> Path -> GenState Path
deviatePath min' max' = mapRandom
  (\(a,b) -> do
      bool <- randomBool
      a'   <- randomFloat min' max'
      b'   <- randomFloat min' max'

      let result | bool      = (a + a', b + b')
                 | otherwise = (a + (a' * 0.3), b + (b' * 0.3))

      return result)
