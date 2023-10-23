module Utils.Collision (circleCollision, shipCollided, projectileCollided, largestRadius) where
import           Graphics.Gloss
import           Model
import           Utils.Lib
import           Utils.PathModels

circleCollision :: Point -> Point -> Path -> Path -> Bool
circleCollision (x1, y1) (x2, y2) p1 p2 = sqrt (x' * x' + y' * y') < r1 + r2
  where
    x' = x1 - x2
    y' = y1 - y2
    r1 = largestRadius p1
    r2 = largestRadius p2

shipCollided :: Player -> GameState -> Bool
shipCollided p gs = any check (asteroids $ world gs) && getCooldown p <= 0
  where
    check :: Asteroid -> Bool
    check (Asteroid path (Pos (Vec2 x'' y'')) _) = circleCollision (x'', y'') (x', y') shipPath path
    Pos (Vec2 x' y') = position p

projectileCollided :: Projectile -> Asteroid -> Bool
projectileCollided (Projectile (Pos pVec) _ _) (Asteroid path (Pos aVec) _) =
  circleCollision (v2ToTuple pVec) (v2ToTuple aVec) [(1,1)] path

largestRadius :: Path -> Float
largestRadius [] = 0
largestRadius [(x',y')] = max (abs x') (abs y')
largestRadius p = maximum [minX, minY, maxX, maxY]
  where
    minX = abs $ minimum $ map fst p
    minY = abs $ minimum $ map snd p
    maxX = abs $ maximum $ map fst p
    maxY = abs $ maximum $ map snd p


