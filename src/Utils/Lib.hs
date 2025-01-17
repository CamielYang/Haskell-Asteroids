module Utils.Lib where
import           Models.Model

degreeToRadian :: Int -> Float
degreeToRadian d = fromIntegral d * pi / 180

degreeToRadianCw :: Int -> Float
degreeToRadianCw d = degreeToRadian (-d + 90)

degreeToVector :: Int -> Vector2
degreeToVector d = Vec2 (cos radians) (sin radians)
  where
    radians :: Float
    radians = degreeToRadianCw d

lengthOfVector :: Vector2 -> Float
lengthOfVector (Vec2 x' y') = sqrt (x' * x' + y' * y')

v2ToTuple :: Vector2 -> (Float, Float)
v2ToTuple (Vec2 x' y') = (x', y')

minMax :: (Ord a) => [a] -> (a, a)
minMax [] = error "minMax: empty list"
minMax [x] = (x, x)
minMax (x:xs) = (min x xsMin, max x xsMax)
  where
    (xsMin, xsMax) = minMax xs
