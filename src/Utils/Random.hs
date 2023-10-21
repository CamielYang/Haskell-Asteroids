module Utils.Random where
import           Graphics.Gloss
import           System.Random

randomInt :: Int -> Int -> IO Int
randomInt x y = getStdRandom (randomR (x,y))

randomFloat :: Float -> Float  -> IO Float
randomFloat x y = getStdRandom (randomR (x,y))

randomBool :: IO Bool
randomBool = do
  num <- randomInt 0 1
  return $ num == 1

deviatePath :: Float -> Float -> Path -> IO Path
deviatePath min max p = mapM f p
  where
    f (a,b) = do
      bool <- randomBool
      a' <- randomFloat min max
      b' <- randomFloat min max
      return $ if bool then (a - a', b - b') else (a - (a' / 3), b - (b' / 3))