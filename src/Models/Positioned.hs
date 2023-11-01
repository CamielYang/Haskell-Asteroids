module Models.Positioned where
import           Models.Model
import           Utils.Lib    (degreeToVector)
import           Utils.Render

withinBox :: Position -> Position
withinBox (Pos (Vec2 x' y'))
  | x' < wl = Pos (Vec2 wr (-y'))
  | x' > wr = Pos (Vec2 wl (-y'))
  | y' < wb = Pos (Vec2 (-x') wt)
  | y' > wt = Pos (Vec2 (-x') wb)
  | otherwise = Pos (Vec2 x' y')
  where
    wl = windowLeft - boundMargin
    wr = windowRight + boundMargin
    wb = windowBottom - boundMargin
    wt = windowTop + boundMargin

class Positioned a where
  getMoveVector :: a -> Vector2
  getMoveVector _ = Vec2 0 0
  getRotation :: a -> Int
  getRotation _ = 0
  getPosition :: a -> Vector2
  move :: a -> Position
  move a = withinBox $ Pos (getPosition a + getMoveVector a)
  setPosition :: a -> Vector2 -> Position
  setPosition a vec = Pos (getPosition a + vec)

instance Positioned Player where
  getMoveVector (Player { velocity = Vel vVec }) = vVec
  getRotation (Player { rotation = Rot r }) = r
  getPosition (Player { position = Pos pv }) = pv

instance Positioned Asteroid where
  getMoveVector (Asteroid _ _ (Rot rot)) = degreeToVector rot
  getRotation (Asteroid _ _ (Rot r)) = r
  getPosition (Asteroid _ (Pos pv) _) = pv

instance Positioned Projectile where
  getMoveVector (Projectile _ (Rot rot) _) = degreeToVector rot * Vec2 projectileSpeed projectileSpeed
  getRotation (Projectile _ (Rot r) _) = r
  getPosition (Projectile (Pos pv) _ _) = pv

instance Positioned Particle where
  getMoveVector (Particle a _) = getMoveVector a * 2
  getRotation (Particle a _) = getRotation a
  getPosition (Particle a _) = getPosition a
