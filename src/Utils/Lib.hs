module Utils.Lib where
import           Graphics.Gloss
import           Model
import           System.Random

drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x,y))

drawFloat :: Float -> Float  -> IO Float
drawFloat x y = getStdRandom (randomR (x,y))

randomBool :: IO Bool
randomBool = do
  num <- drawInt 0 1
  return $ num == 1

randomizeCoords :: [Point] -> IO [Point]
randomizeCoords = mapM f
  where
    f (a,b) = do
      bool <- randomBool
      a' <- drawFloat 0.5 1
      b' <- drawFloat 0.5 1
      return $ if bool then (a - a', b - b') else (a - (a' / 3), b - (b' / 3))

updateRotation :: Player -> Int -> Rotation
updateRotation (Player { rotation = Rot r }) d = Rot (r + d)

degreeToRadian :: Int -> Float
degreeToRadian d = fromIntegral d * pi / 180

degreeToRadianCw :: Int -> Float
degreeToRadianCw d = degreeToRadian (-d + 90)

degreeToVector :: Int -> Vector2
degreeToVector d = Vector2 (cos radians) (sin radians)
  where
    radians :: Float
    radians = degreeToRadianCw d

lengthOfVector :: Vector2 -> Float
lengthOfVector (Vector2 x' y') = sqrt (x' * x' + y' * y')
