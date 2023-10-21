module Utils.Keys where
import qualified Data.Set                         as S
import           Graphics.Gloss.Interface.IO.Game

disableKeys :: S.Set Key -> [Key] -> S.Set Key
disableKeys s [] = s
disableKeys s (k:ks)
  | S.member k s = disableKeys (S.delete k s) ks
  | otherwise = disableKeys s ks
