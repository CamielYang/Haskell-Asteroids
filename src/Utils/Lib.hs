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

setRotation :: Player -> Int -> Rotation
setRotation (Player { rotation = Rot r }) d
  | r + d > 360 = Rot (r + d - 360)
  | r + d < 0   = Rot (r + d + 360)
  | otherwise   = Rot (r + d)

setVelocity :: Player -> Float -> Velocity
setVelocity p@(Player { velocity = Vel vVec }) v = if lengthOfVector newV2 > maxVelocity
  then velocity p
  else Vel newV2
  where
    Rot d   = rotation p
    dirVec  = degreeToVector d * Vec2 v v
    dragVec = Vec2 drag drag
    newV2   = (vVec + dirVec) * dragVec

