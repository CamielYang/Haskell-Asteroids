module Utils.Render where
import           Graphics.Gloss
import           Utils.PathModels
import           Utils.Point

width, height :: Float
width = 1500 
height = 750

windowLeft, windowRight, windowTop, windowBottom :: Float
windowLeft   = (-width) / 2
windowRight  = width / 2
windowTop    = height / 2
windowBottom = (-height) / 2

renderText :: String -> Float -> Float -> Float -> Picture
renderText t x' y' s = translate x' y' $ scale s s $ color white $ text t

renderSpaceShip :: Color -> Picture
renderSpaceShip c = color c $ lineLoop shipPath

renderSpaceShip' :: Picture
renderSpaceShip' = color white $ lineLoop $ rotatePath 45 shipPath

renderAsteroid :: Path -> Picture
renderAsteroid p = color white $ lineLoop p
