module Display.MainBoard (display) where
-- ^
-- import qualified Display.MainBoard as MB

import Display.Common
import Enum.Enum
import Core.MainBoard (MainBoard, Depot(..))
import qualified Core.MainBoard as MB
import Builtin

import Data.Fugue (replaceAll, padList)
import Text.Printf

d2hs :: MainBoard -> Depot -> [Maybe HexTile]
d2hs m d@(Depot _) = padList (MB.market m d) 4
d2hs m d@(BlackDepot) = padList (MB.market m d) 8

d2gstring :: MainBoard -> Depot -> String
d2gstring _ BlackDepot = []
d2gstring m d@(Depot _) = concat (g2i <$> MB.warehouse m d)

display :: MainBoard -> String
display m = replaceAll raw subs where
    subs = hs ++ bhs ++ gs
    hs = zip
        ([printf "{d%d%d}" (a::Int) (b::Int) | a <- [1..6], b <- [1..4]] :: [String])
        (mht2i <$> concat [d2hs m (Depot $ Dice d) | d <- [1..6]])
    bhs = zip
        ([printf "{db%d}" (b::Int) | b <- [1..8]] :: [String])
        (mht2i <$> d2hs m BlackDepot)
    gs = zip
        ([printf "{g%d}" (b::Int) | b <- [1..6]] :: [String])
        ([d2gstring m (Depot $ Dice d) | d <- [1..6]])

raw :: String
raw =
    "Depot | HexTiles                 | Goods                                    \n\
    \-----------------------------------------                                   \n\
    \ ___  |   / \\   / \\   / \\   / \\  |                                          \n\
    \|   | |  /   \\ /   \\ /   \\ /   \\ |                                          \n\
    \| . | | |{d11}|{d12}|{d13}|{d14}|| {g1}                                     \n\
    \|   | |  \\   / \\   / \\   / \\   / |                                          \n\
    \ ---  |   \\./   \\./   \\./   \\./  |_______                                   \n\
    \ ___  |   / \\   / \\   / \\   / \\  |                                          \n\
    \|  .| |  /   \\ /   \\ /   \\ /   \\ |                                          \n\
    \|   | | |{d21}|{d22}|{d23}|{d24}|| {g2}                                     \n\
    \|.  | |  \\   / \\   / \\   / \\   / |                                          \n\
    \ ---  |   \\./   \\./   \\./   \\./  |_______                                   \n\
    \ ___  |   / \\   / \\   / \\   / \\  |                                          \n\
    \|  .| |  /   \\ /   \\ /   \\ /   \\ |                                          \n\
    \| . | | |{d31}|{d32}|{d33}|{d34}|| {g3}                                     \n\
    \|.  | |  \\   / \\   / \\   / \\   / |                                          \n\
    \ ---  |   \\./   \\./   \\./   \\./  |_______                                   \n\
    \ ___  |   / \\   / \\   / \\   / \\  |                                          \n\
    \|. .| |  /   \\ /   \\ /   \\ /   \\ |                                          \n\
    \|   | | |{d41}|{d42}|{d43}|{d44}|| {g4}                                     \n\
    \|. .| |  \\   / \\   / \\   / \\   / |                                          \n\
    \ ---  |   \\./   \\./   \\./   \\./  |_______                                   \n\
    \ ___  |   / \\   / \\   / \\   / \\  |                                          \n\
    \|. .| |  /   \\ /   \\ /   \\ /   \\ |                                          \n\
    \| . | | |{d51}|{d52}|{d53}|{d54}|| {g5}                                     \n\
    \|. .| |  \\   / \\   / \\   / \\   / |                                          \n\
    \ ---  |   \\./   \\./   \\./   \\./  |_______                                   \n\
    \ ___  |   / \\   / \\   / \\   / \\  |                                          \n\
    \|. .| |  /   \\ /   \\ /   \\ /   \\ |                                          \n\
    \|. .| | |{d61}|{d62}|{d63}|{d64}|| {g6}                                     \n\
    \|. .| |  \\   / \\   / \\   / \\   / |                                          \n\
    \ ---  |   \\./   \\./   \\./   \\./  |_____________________                     \n\
    \ ___  |   / \\   / \\   / \\   / \\   / \\   / \\   / \\   / \\                     \n\
    \|XXX| |  /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\                    \n\
    \|XXX| | |{db1}|{db2}|{db3}|{db4}|{db5}|{db6}|{db7}|{db8}|                   \n\
    \|XXX| |  \\   / \\   / \\   / \\   / \\   / \\   / \\   / \\   /                    \n\
    \ ---  |   \\./   \\./   \\./   \\./   \\./   \\./   \\./   \\./                     \n"


--------------------------------------------------------------------------------
--This is the predoctored string
--Select all of this and run :s/\\/\\\\/g, then block select the first column
--and add 4 space and \, and then change the first column of \ to \n
--Finally, change the first and last \ to "
--
-- Depot | HexTiles                 | Goods                                    \
-- -----------------------------------------                                   \
--  ___  |   / \   / \   / \   / \  |                                          \
-- |   | |  /   \ /   \ /   \ /   \ |                                          \
-- | . | | |{d11}|{d12}|{d13}|{d14}|| {g1}                                     \
-- |   | |  \   / \   / \   / \   / |                                          \
--  ---  |   \ /   \ /   \ /   \ /  |_______                                   \
--  ___  |   / \   / \   / \   / \  |                                          \
-- |  .| |  /   \ /   \ /   \ /   \ |                                          \
-- |   | | |{d21}|{d22}|{d23}|{d24}|| {g2}                                     \
-- |.  | |  \   / \   / \   / \   / |                                          \
--  ---  |   \ /   \ /   \ /   \ /  |_______                                   \
--  ___  |   / \   / \   / \   / \  |                                          \
-- |  .| |  /   \ /   \ /   \ /   \ |                                          \
-- | . | | |{d31}|{d32}|{d33}|{d34}|| {g3}                                     \
-- |.  | |  \   / \   / \   / \   / |                                          \
--  ---  |   \ /   \ /   \ /   \ /  |_______                                   \
--  ___  |   / \   / \   / \   / \  |                                          \
-- |. .| |  /   \ /   \ /   \ /   \ |                                          \
-- |   | | |{d41}|{d42}|{d43}|{d44}|| {g4}                                     \
-- |. .| |  \   / \   / \   / \   / |                                          \
--  ---  |   \ /   \ /   \ /   \ /  |_______                                   \
--  ___  |   / \   / \   / \   / \  |                                          \
-- |. .| |  /   \ /   \ /   \ /   \ |                                          \
-- | . | | |{d51}|{d52}|{d53}|{d54}|| {g5}                                     \
-- |. .| |  \   / \   / \   / \   / |                                          \
--  ---  |   \ /   \ /   \ /   \ /  |_______                                   \
--  ___  |   / \   / \   / \   / \  |                                          \
-- |. .| |  /   \ /   \ /   \ /   \ |                                          \
-- |. .| | |{d61}|{d62}|{d63}|{d64}|| {g6}                                     \
-- |. .| |  \   / \   / \   / \   / |                                          \
--  ---  |   \ /   \ /   \ /   \ /  |_____________________                     \
--  ___  |   / \   / \   / \   / \   / \   / \   / \   / \                     \
-- |XXX| |  /   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \                    \
-- |XXX| | |{db1}|{db2}|{db3}|{db4}|{db1}|{db2}|{db3}|{db4}|                   \
-- |XXX| |  \   / \   / \   / \   / \   / \   / \   / \   /                    \
--  ---  |   \ /   \ /   \ /   \ /   \ /   \ /   \ /   \ /                     \
