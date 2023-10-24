module Models.Collidable where
import           Graphics.Gloss
import           Models.Model
import           Models.ModelLib
import           Models.Positioned

largestRadius :: Path -> Float
largestRadius [] = 0
largestRadius [(x',y')] = max (abs x') (abs y')
largestRadius p = maximum [minX, minY, maxX, maxY]
  where
    minX = abs $ minimum $ map fst p
    minY = abs $ minimum $ map snd p
    maxX = abs $ maximum $ map fst p
    maxY = abs $ maximum $ map snd p

class (Positioned a) => Collidable a where
  getHitboxRadius :: a -> Float
  isColliding :: (Collidable b) => a -> b -> Bool
  isColliding a b = sqrt (x' + y') < radius
    where
      Vec2 x' y' = (getPosition a - getPosition b) ^ (2 :: Integer)
      radius = getHitboxRadius a + getHitboxRadius b

instance Collidable Player where
  getHitboxRadius p = largestRadius (path p)

instance Collidable Asteroid where
  getHitboxRadius (Asteroid p _ _) = largestRadius p

instance Collidable Projectile where
  getHitboxRadius _ = 1

shipCollided :: Player -> GameState -> Bool
shipCollided p gs = any (isColliding p) (asteroids $ world gs) && getCooldown p <= 0
