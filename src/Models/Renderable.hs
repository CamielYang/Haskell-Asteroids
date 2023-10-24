module Models.Renderable where
import           Graphics.Gloss
import           Models.Model
import           Models.ModelLib
import           Utils.Render    (renderSpaceShip)

class Renderable a where
  render :: a -> Picture
  renderMap :: [a] -> Picture
  renderMap = Pictures . map render

instance Renderable Player where
  render p@(Player { cooldown = Time cd})
    | isKilled p = blank
    | otherwise  = translate x' y'
                   $ rotate (fromIntegral r)
                   $ renderSpaceShip c
    where
      Pos (Vec2 x' y') = position p
      Rot r            = rotation p
      c
        | cd <= 0    = pColor p
        | otherwise  = withAlpha 0.5 $ pColor p

instance Renderable Asteroid where
  render (Asteroid p (Pos (Vec2 x' y')) _) = translate x' y' $ color white $ lineLoop p

instance Renderable Projectile where
  render (Projectile (Pos (Vec2 x' y')) _ _) = translate x' y' $ color white $ circleSolid 2

