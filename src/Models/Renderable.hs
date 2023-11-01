module Models.Renderable where
import           Data.Fixed        (mod')
import           Graphics.Gloss
import           Models.Model
import           Models.Positioned
import           Models.SpaceShip
import           Utils.Render      (renderSpaceShip)

class (Positioned a) => Renderable a where
  getPicture :: a -> Picture
  getColor :: a -> Color
  getColor _ = white
  transform :: a -> Picture -> Picture
  transform a = translate x' y' . rotate (fromIntegral r)
    where
      Vec2 x' y' = getPosition a
      r            = getRotation a
  render :: a -> Picture
  render a = color (getColor a) $ transform a $ getPicture a
  renderMap :: [a] -> Picture
  renderMap = Pictures . map render

instance Renderable Player where
  getColor p@(Player { health = HP _ (Time cd)}) = c
    where
      c | cd <= 0 || cd `mod'` 1 < 0.5 = pColor p
        | otherwise  = withAlpha 0.5 $ pColor p
  getPicture p = renderSpaceShip $ getColor p
  render p
    | isKilled p = blank
    | otherwise  = transform p $ getPicture p

instance Renderable Asteroid where
  getPicture (Asteroid p _ _) = lineLoop p

instance Renderable Projectile where
  getPicture _ = circleSolid 2
