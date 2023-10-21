module Utils.Collision (circleCollision, largestDistance) where
import           Graphics.Gloss

circleCollision :: Point -> Point -> Path -> Path -> Bool
circleCollision (x1, y1) (x2, y2) p1 p2 = sqrt (x * x + y * y) < r1 + r2
  where
    x = x1 - x2
    y = y1 - y2
    r1 = largestDistance p1
    r2 = largestDistance p2

largestDistance :: Path -> Float
largestDistance [] = 0
largestDistance [(x,y)] = max (abs x) (abs y)
largestDistance p = maximum [minX, minY, maxX, maxY]
  where
    minX = abs $ minimum $ map fst p
    minY = abs $ minimum $ map snd p
    maxX = abs $ maximum $ map fst p
    maxY = abs $ maximum $ map snd p
