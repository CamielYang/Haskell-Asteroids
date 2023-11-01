module Utils.Render where
import           Graphics.Gloss
import           Models.Model

windowLeft, windowRight, windowTop, windowBottom :: Float
windowLeft   = (-width) / 2
windowRight  = width / 2
windowTop    = height / 2
windowBottom = (-height) / 2

renderText :: String -> Float -> Float -> Float -> Picture
renderText t x' y' s = translate x' y' $ scale s s $ color white $ text t
