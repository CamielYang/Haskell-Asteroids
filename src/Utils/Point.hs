module Utils.Point where

import           Graphics.Gloss
import           Utils.Lib

rotatePointCW :: Int -> Point -> Point
rotatePointCW d (x,y) = (x * cos rad - y * sin rad
                    ,x * sin rad + y * cos rad)
                    where
                      rad = degreeToRadian (-d)

rotatePath :: Int -> Path -> Path
rotatePath d = map (rotatePointCW d)

translatePath :: Point -> Path -> Path
translatePath (x', y') = map (\(x,y) -> (x + x', y + y'))

scalePath :: Float -> Path -> Path
scalePath s = map (\(x,y) -> (x * s, y * s))
