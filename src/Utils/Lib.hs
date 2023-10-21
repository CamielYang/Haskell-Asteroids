module Utils.Lib where
import           Model

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
