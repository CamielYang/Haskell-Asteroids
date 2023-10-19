module Lib where
import           System.Random

drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x,y))

drawFloat :: Float -> Float  -> IO Float
drawFloat x y = getStdRandom (randomR (x,y))

randomBool :: IO Bool
randomBool = do
  num <- drawInt 0 1
  return $ num == 1
