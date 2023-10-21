module Utils.Point where

import           Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Graphics.Gloss.Data.Vector           (magV)
import           Utils.Lib

rotateVCW :: Int -> Point -> Point
rotateVCW d (x,y) = (x * cos rad - y * sin rad
                    ,x * sin rad + y * cos rad)
                    where
                      rad = degreeToRadian (-d)

rotatePath :: Int -> [Point] -> [Point]
rotatePath d = map (rotateVCW d)

translatePath :: Point -> [Point] -> [Point]
translatePath (x', y') = map (\(x,y) -> (x + x', y + y'))

scalePath :: Float -> [Point] -> [Point]
scalePath s = map (\(x,y) -> (x * s, y * s))
