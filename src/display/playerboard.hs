module Display.PlayerBoard (display) where

import Core.PlayerBoard
import Builtin
import Enum.Enum
import Enum.Hex

import Display.Common

import Data.Fugue (replaceAll, padList)
--import Text.Regex (mkRegex, subRegex)
import Text.Printf
import Data.List (group, sort, find, deleteBy)

h2i :: PlayerBoard -> Hex -> String
h2i p h = case lattice p h of
    Just h -> ht2i h
    Nothing -> case layout p h of
        Just s -> s2i s
        Nothing -> "ERR"

ps2i :: PlayerBoard -> Hex -> String
ps2i p h = case lattice p h of
    Just h -> "   "
    Nothing -> case layout p h of
        Just s -> s2d s
        Nothing -> "ERR"

--PlayerBoard Slot to Initials
s2i :: Slot -> String
s2i Slot{color = Burgundy} = " c "
s2i Slot{color = Silver} = " m "
s2i Slot{color = Blue} = " s "
s2i Slot{color = Green} = " p "
s2i Slot{color = Brown} = " b "
s2i Slot{color = Yellow} = " k "

--Slot to dice
s2d :: Slot -> String
s2d Slot{dice = Dice i} = " " ++ show i ++ " "

--Goods range to intials
gr2i :: Maybe [GoodsTile] -> String
gr2i Nothing = "   "
gr2i (Just gs) = "x" ++ show (length gs) ++ " "

--storages to initials
ss2i :: Maybe HexTile -> String
ss2i Nothing = "   "
ss2i (Just h) = ht2i h

--Action to Initials
ac2i :: Maybe DiceAction -> String
ac2i Nothing = "    "
ac2i (Just (Standard (Dice i))) = "D" ++ show i ++ "  "
ac2i (Just Free) = "Free"
ac2i (Just (Pull (Brown:_))) = "PB  "
ac2i (Just (Pull (Burgundy:_))) = "PCMK"
ac2i (Just (Pull (Silver:_))) = "PCMK"
ac2i (Just (Pull (Yellow:_))) = "PCMK"
ac2i (Just (Pull (Green:_))) = "PPS "
ac2i (Just (Pull (Blue:_))) = "PPS "
ac2i (Just (Pull [])) = "ERR"
ac2i (Just Push) = "Push"
ac2i (Just Ship) = "Ship"
ac2i (Just DrawGoods) = "Draw"
ac2i (Just Buy) = "Buy "

--BonusTile to Intials
bt2i :: Maybe BonusTile -> String
bt2i Nothing = " "
bt2i (Just (BonusTile Burgundy BigBonus)) = "C"
bt2i (Just (BonusTile Burgundy SmallBonus)) = "c"
bt2i (Just (BonusTile Silver BigBonus)) = "M"
bt2i (Just (BonusTile Silver SmallBonus)) = "m"
bt2i (Just (BonusTile Blue BigBonus)) = "S"
bt2i (Just (BonusTile Blue SmallBonus)) = "s"
bt2i (Just (BonusTile Green BigBonus)) = "G"
bt2i (Just (BonusTile Green SmallBonus)) = "g"
bt2i (Just (BonusTile Brown BigBonus)) = "B"
bt2i (Just (BonusTile Brown SmallBonus)) = "b"
bt2i (Just (BonusTile Yellow BigBonus)) = "Y"
bt2i (Just (BonusTile Yellow SmallBonus)) = "y"

--Return True if the dice action is a candidate for {im}
isImmediate :: DiceAction -> Bool
isImmediate (Standard _) = False
isImmediate Buy = False
isImmediate Free = True
isImmediate (Pull _) = True
isImmediate Push = True
isImmediate Ship = True
isImmediate DrawGoods = True

--Return True if the dice action is stardard
isStandard :: DiceAction -> Bool
isStandard (Standard _) = True
isStandard _ = False

--Return True if the dice action is buy
isBuy :: DiceAction -> Bool
isBuy Buy = True
isBuy _ = False


--All hail the spghetti monster!
display :: PlayerBoard -> String
display p = replaceAll raw subs where
    subs = cs ++ ds ++ gs ++ xs ++ hs ++ ss ++ bonus ++ as ++ other
    cs = zip
        ([printf "c%02d" (n::Int) | n <- [0..36]])
        (h2i p <$> sortedRange)
    ds = zip
        ([printf "d%02d" (n::Int) | n <- [0..36]])
        (ps2i p <$> sortedRange)
    gs = zip
        ([printf "g%02d" (n::Int) | n <- [1..3]])
        (mg2i <$> (padList (head <$> (group . sort $ dock p)) 3))
    xs = zip
        ([printf "x%02d" (n::Int) | n <- [1..3]])
        (gr2i <$> (padList (group . sort $ dock p) 3))
    goodsList = Dice <$> [1..6] >>= return . GoodsTile
    hs = zip
        ([printf "h%02d" (n::Int) | n <- [1..6]])
        ((\i -> " " ++ show i ++ " ") <$> ((\g -> (length . filter (== g)) (shipped p)) <$> goodsList))
    ss = zip
        ([printf "s%02d" (n::Int) | n <- [1..3]])
        (ss2i <$> padList (storage p) 3)
    bonusList = [BonusTile c t
        | t <- [BigBonus, SmallBonus]
        , c <- ([minBound..maxBound] :: [Color])]
    bonus =
        [ ("{bonusTiles}", (\b -> find (== b) $ bonusTiles p) <$> bonusList >>= bt2i)]
    as =
        [ ("{im}", ac2i $ find isImmediate $ actions p)
        , ("{a1}", ac2i $ find (isStandard) $ actions p)
        , ("{a2}", ac2i $ find (isStandard) $ deleteBy (\_ -> isStandard) Free $ actions p) --This is really hacky and should be fixed asap
        , ("{bu}", ac2i $ find isBuy $ actions p)
        ]
    other =
        [ ("{sc}", printf "%02d  " $ silverlingCount p)
        , ("{wc}", printf "%02d  " $ workerCount p)
        , ("{vt}", printf "%03d " $ victoryTrack p)
        ]

--The string used by display, with patterns to be replaced
raw :: String
raw =
    "  Dock:                     / \\   / \\   / \\   / \\            Actions:        \n\
    \   --- --- ---             /   \\ /   \\ /   \\ /   \\           {im}            \n\
    \  |g01|g02|g03|           | c00 | c01 | c02 | c03 |          {a1}            \n\
    \   --- --- ---           / \\d00/ \\d01/ \\d02/ \\d03/ \\         {a2}            \n\
    \   x01 x02 x03          /   \\ /   \\ /   \\ /   \\ /   \\        {bu}            \n\
    \                       | c04 | c05 | c06 | c07 | c08 |                       \n\
    \   Shipped:           / \\d04/ \\d05/ \\d06/ \\d07/ \\d08/ \\                      \n\
    \   --- --- ---       /   \\ /   \\ /   \\ /   \\ /   \\ /   \\                     \n\
    \  |h01|h02|h03|     | c09 | c10 | c11 | c12 | c13 | c14 |                    \n\
    \   --- --- ---     / \\d09/ \\d10/ \\d11/ \\d12/ \\d13/ \\d14/ \\                   \n\
    \  |h04|h05|h06|   /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\                  \n\
    \   --- --- ---   | c15 | c16 | c17 | c18 | c19 | c20 | c21 |                 \n\
    \                  \\d15/ \\d16/ \\d17/ \\d18/ \\d19/ \\d20/ \\d21/                  \n\
    \                   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /                   \n\
    \  Silverlings: {sc} | c22 | c23 | c24 | c25 | c26 | c27 |                    \n\
    \      Workers: {wc}  \\d22/ \\d23/ \\d24/ \\d25/ \\d26/ \\d27/                     \n\
    \ VictoryTrack: {vt}   \\ /   \\ /   \\ /   \\ /   \\ /   \\ /                      \n\
    \                       | c28 | c29 | c30 | c31 | c32 |                       \n\
    \    / \\   / \\   / \\     \\d28/ \\d29/ \\d30/ \\d31/ \\d32/                        \n\
    \   /   \\ /   \\ /   \\     \\ /   \\ /   \\ /   \\ /   \\ /   BonusTiles:           \n\
    \  | s01 | s02 | s03 |     | c33 | c34 | c35 | c36 |    {bonusTiles}          \n\
    \   \\   / \\   / \\   /       \\d33/ \\d34/ \\d35/ \\d36/                           \n\
    \    \\ /   \\ /   \\ /         \\ /   \\ /   \\ /   \\ /                            \n"

--------------------------------------------------------------------------------
--This is the predoctored string
--Select all of this and run :s/\\/\\\\/g, then block select the first column
--and add 4 space and /, and then change the first column of \ to \n
--Finally, change the first and last \ to "
--  Dock:                     / \   / \   / \   / \            Actions:        \
--   --- --- ---             /   \ /   \ /   \ /   \           {im}            \
--  |g01|g02|g03|           | c00 | c01 | c02 | c03 |          {a1}            \
--   --- --- ---           / \d00/ \d01/ \d02/ \d03/ \         {a2}            \
--   x01 x02 x03          /   \ /   \ /   \ /   \ /   \        {bu}            \
--                       | c04 | c05 | c06 | c07 | c08 |                       \
--   Shipped:           / \d04/ \d05/ \d06/ \d07/ \d08/ \                      \
--   --- --- ---       /   \ /   \ /   \ /   \ /   \ /   \                     \
--  |h01|h02|h03|     | c09 | c10 | c11 | c12 | c13 | c14 |                    \
--   --- --- ---     / \d09/ \d10/ \d11/ \d12/ \d13/ \d14/ \                   \
--  |h04|h05|h06|   /   \ /   \ /   \ /   \ /   \ /   \ /   \                  \
--   --- --- ---   | c15 | c16 | c17 | c18 | c19 | c20 | c21 |                 \
--                  \d15/ \d16/ \d17/ \d18/ \d19/ \d20/ \d21/                  \
--                   \ /   \ /   \ /   \ /   \ /   \ /   \ /                   \
--  Silverlings: {sc} | c22 | c23 | c24 | c25 | c26 | c27 |                    \
--      Workers: {wc}  \d22/ \d23/ \d24/ \d25/ \d26/ \d27/                     \
-- VictoryTrack: {vt}   \ /   \ /   \ /   \ /   \ /   \ /                      \
--                       | c28 | c29 | c30 | c31 | c32 |                       \
--    / \   / \   / \     \d28/ \d29/ \d30/ \d31/ \d32/                        \
--   /   \ /   \ /   \     \ /   \ /   \ /   \ /   \ /   BonusTiles:           \
--  | s01 | s02 | s03 |     | c33 | c34 | c35 | c36 |    {bonusTiles}          \
--   \   / \   / \   /       \d33/ \d34/ \d35/ \d36/                           \
--    \ /   \ /   \ /         \ /   \ /   \ /   \ /                            \

--------------------------------------------------------------------------------
--This is an example instance of display p (its a touch outdated, and I don't feel like replacing it)
--------------------------------------------------------------------------------
--  Dock                     / \   / \   / \   / \         Shipped:
--   --- --- ---            /   \ /   \ /   \ /   \        --- --- ---
--  | 3 | 6 |   |          |  p  |  c  |  c  |  k  |      | 3 | 1 | 0 |
--   --- --- ---          / \ 6 / \ 5 / \ 4 / \ 3 / \      --- --- ---
--   x2  x1              /   \ /   \ /   \ /   \ /   \    | 0 | 2 | 0 |
--                      |  p  |  p  |  c  |  k  |  b  |    --- --- ---
--                     / \ 2 / \ 1 / \ 6 / \ 5 / \ 4 / \
--                    /   \ /   \ /   \ /   \ /   \ /   \
--                   |  p  |  p  |  b  |  k  |  b  |  b  |
--                  / \ 5 / \ 4 / \ 3 / \ 1 / \ 2 / \ 3 / \
--                 /   \ /   \ /   \ / C \ /   \ /   \ /   \
--                |  s  |  s  |  s  | CAS |  s  |  s  |  s  |
--                 \ 6 / \ 1 / \ 2 / \ C / \ 5 / \ 4 / \ 1 /
--                  \ /   \ /   \ /   \ /   \ /   \ /   \ /
-- Silverlings: 2    |  b  |  b  |  m  |  b  |  b  |  p  |
--     Workers: 5     \ 2 / \ 5 / \ 4 / \ 3 / \ 1 / \ 2 /
--VictoryTrack: 30     \ /   \ /   \ /   \ /   \ /   \ /
--                      |  b  |  m  |  k  |  b  |  b  |
--   / \   / \   / \     \ 6 / \ 1 / \ 2 / \ 5 / \ 6 /
--  /   \ /   \ /   \     \ /   \ /   \ /   \ /   \ /
-- | CAS | K20 |     |     |  m  |  k  |  k  |  b  |
--  \   / \   / \   /       \ 3 / \ 4 / \ 1 / \ 3 /
--   \ /   \ /   \ /         \ /   \ /   \ /   \ /
