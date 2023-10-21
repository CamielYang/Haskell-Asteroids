module Utils.ViewLib where
import           Graphics.Gloss
import           Model
import           Utils.Lib
import           Utils.Path
import           Utils.Point

width, height :: Int
width = 800
height = 600

windowLeft, windowRight, windowTop, windowBottom :: Float
windowLeft = fromIntegral (-width) / 2
windowRight = fromIntegral width / 2
windowTop = fromIntegral height / 2
windowBottom = fromIntegral (-height) / 2

renderText :: String -> Float -> Float -> Float -> Float -> Picture
renderText t x' y' sx sy = translate x' y' $ scale sx sy $ color white $ text t

renderSpaceShip :: Color -> Picture
renderSpaceShip c = color c $ lineLoop shipPath

renderSpaceShip' :: Picture
renderSpaceShip' = color white $ lineLoop $ rotatePath 45 shipPath

renderAsteroid :: Path -> Picture
renderAsteroid p = color white $ lineLoop p


getRotation :: Player -> Int
getRotation (Player { rotation = Rot r }) = r

getHp :: Player -> Int
getHp (Player { health = HP hp }) = hp
