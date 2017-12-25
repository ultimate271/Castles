module Display.Common where
-- ^
-- import           Display.Common

import Builtin
import Enum.Enum
import Text.Printf

--Hex Tile To Initials
ht2i :: HexTile -> String
ht2i Castle = "CAS"
ht2i Mine = "MIN"
ht2i Boat = "BOT"
ht2i (Pasture Cow x) = "PC" ++ show x
ht2i (Pasture Sheep x) = "PS" ++ show x
ht2i (Pasture Pig x) = "PP" ++ show x
ht2i (Pasture Chicken x) = "PH" ++ show x
ht2i (Building b) = printf "B%02d" $ fromEnum b
ht2i (Knowledge k) = printf "K%02d" $ fromEnum k

--Maybe Hex Tile to Initials
mht2i :: Maybe HexTile -> String
mht2i Nothing = "     "
mht2i (Just h) = " " ++ ht2i h ++ " "

--Goods to Initials
g2i :: GoodsTile -> String
g2i (GoodsTile (Dice i)) = " " ++ show i ++ " "

--Maybe Goods to Initials
mg2i :: Maybe GoodsTile -> String
mg2i Nothing = "   "
mg2i (Just g) = g2i g

