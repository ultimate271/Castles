module Builtin where

import Enum.Enum
import qualified Enum.Hex as Hex
import qualified Enum.Config as CFG
import qualified Core.PlayerBoard as PB
import qualified Core.MainBoard as MB
import qualified Core.State as S

import Data.List (sortBy)

standardConfig :: Int -> CFG.Config
standardConfig p = CFG.Config
    { CFG.storageSize = 3
    , CFG.dockSize = 3
    , CFG.hexRadius = 3
    , CFG.playerCount = p
    , CFG.diceSize = 6
    , CFG.phaseCount = 5
    , CFG.turnsPerPhase = 5
    }

standardMainBoard :: Int -> MB.MainBoard
-- ^vanilla main board with n players
standardMainBoard 2 = MB.build MB.blank
    [ MB.addToLayout MB.Slot{MB.color = Just Blue     , MB.depot = Depot $ Dice 1}
    , MB.addToLayout MB.Slot{MB.color = Just Brown    , MB.depot = Depot $ Dice 1}
    , MB.addToLayout MB.Slot{MB.color = Just Burgundy , MB.depot = Depot $ Dice 2}
    , MB.addToLayout MB.Slot{MB.color = Just Yellow   , MB.depot = Depot $ Dice 2}
    , MB.addToLayout MB.Slot{MB.color = Just Green    , MB.depot = Depot $ Dice 3}
    , MB.addToLayout MB.Slot{MB.color = Just Brown    , MB.depot = Depot $ Dice 3}
    , MB.addToLayout MB.Slot{MB.color = Just Blue     , MB.depot = Depot $ Dice 4}
    , MB.addToLayout MB.Slot{MB.color = Just Brown    , MB.depot = Depot $ Dice 4}
    , MB.addToLayout MB.Slot{MB.color = Just Silver   , MB.depot = Depot $ Dice 5}
    , MB.addToLayout MB.Slot{MB.color = Just Yellow   , MB.depot = Depot $ Dice 5}
    , MB.addToLayout MB.Slot{MB.color = Just Green    , MB.depot = Depot $ Dice 6}
    , MB.addToLayout MB.Slot{MB.color = Just Brown    , MB.depot = Depot $ Dice 6}
    , MB.addToLayout MB.Slot{MB.color = Nothing       , MB.depot = BlackDepot}
    , MB.addToLayout MB.Slot{MB.color = Nothing       , MB.depot = BlackDepot}
    , MB.addToLayout MB.Slot{MB.color = Nothing       , MB.depot = BlackDepot}
    , MB.addToLayout MB.Slot{MB.color = Nothing       , MB.depot = BlackDepot}
    ]
standardMainBoard 3 = MB.build (standardMainBoard 2)
    [ MB.addToLayout MB.Slot{MB.color = Just Yellow   , MB.depot = Depot $ Dice 1}
    , MB.addToLayout MB.Slot{MB.color = Just Brown    , MB.depot = Depot $ Dice 2}
    , MB.addToLayout MB.Slot{MB.color = Just Blue     , MB.depot = Depot $ Dice 3}
    , MB.addToLayout MB.Slot{MB.color = Just Green    , MB.depot = Depot $ Dice 4}
    , MB.addToLayout MB.Slot{MB.color = Just Brown    , MB.depot = Depot $ Dice 5}
    , MB.addToLayout MB.Slot{MB.color = Just Burgundy , MB.depot = Depot $ Dice 6}
    , MB.addToLayout MB.Slot{MB.color = Nothing       , MB.depot = BlackDepot}
    , MB.addToLayout MB.Slot{MB.color = Nothing       , MB.depot = BlackDepot}
    ]
standardMainBoard 4 = MB.build (standardMainBoard 3)
    [ MB.addToLayout MB.Slot{MB.color = Just Green  , MB.depot = Depot $ Dice 1}
    , MB.addToLayout MB.Slot{MB.color = Just Brown  , MB.depot = Depot $ Dice 2}
    , MB.addToLayout MB.Slot{MB.color = Just Yellow , MB.depot = Depot $ Dice 3}
    , MB.addToLayout MB.Slot{MB.color = Just Silver , MB.depot = Depot $ Dice 4}
    , MB.addToLayout MB.Slot{MB.color = Just Brown  , MB.depot = Depot $ Dice 5}
    , MB.addToLayout MB.Slot{MB.color = Just Blue   , MB.depot = Depot $ Dice 6}
    , MB.addToLayout MB.Slot{MB.color = Nothing     , MB.depot = BlackDepot}
    , MB.addToLayout MB.Slot{MB.color = Nothing     , MB.depot = BlackDepot}
    ]

sortedRange :: [Hex.Hex]
sortedRange = sortBy sorter $ Hex.range (Hex.toAxial Hex.center) 3
  where sorter :: Hex.Hex -> Hex.Hex -> Ordering
        sorter (Hex.Axial x y) (Hex.Axial x' y') = let z = -x-y; z' = -x'-y' in
            if compare z z' /= EQ then compare z z' else compare y' y
        sorter i j = sorter (Hex.toAxial i) (Hex.toAxial j)

standardSlots :: [PB.Slot]
standardSlots =
    --Row 3
    [ PB.Slot{PB.color = Green, PB.dice = Dice 6}
    , PB.Slot{PB.color = Burgundy, PB.dice = Dice 5}
    , PB.Slot{PB.color = Burgundy, PB.dice = Dice 4}
    , PB.Slot{PB.color = Yellow, PB.dice = Dice 3}
    --Row 2
    , PB.Slot{PB.color = Green, PB.dice = Dice 2}
    , PB.Slot{PB.color = Green, PB.dice = Dice 1}
    , PB.Slot{PB.color = Burgundy, PB.dice = Dice 6}
    , PB.Slot{PB.color = Yellow, PB.dice = Dice 5}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 4}
    --Row 1
    , PB.Slot{PB.color = Green, PB.dice = Dice 5}
    , PB.Slot{PB.color = Green, PB.dice = Dice 4}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 3}
    , PB.Slot{PB.color = Yellow, PB.dice = Dice 1}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 2}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 3}
    --Row 0
    , PB.Slot{PB.color = Blue, PB.dice = Dice 6}
    , PB.Slot{PB.color = Blue, PB.dice = Dice 1}
    , PB.Slot{PB.color = Blue, PB.dice = Dice 2}
    , PB.Slot{PB.color = Green, PB.dice = Dice 6}
    , PB.Slot{PB.color = Blue, PB.dice = Dice 5}
    , PB.Slot{PB.color = Blue, PB.dice = Dice 4}
    , PB.Slot{PB.color = Blue, PB.dice = Dice 1}
    --Row -1
    , PB.Slot{PB.color = Brown, PB.dice = Dice 2}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 5}
    , PB.Slot{PB.color = Silver, PB.dice = Dice 4}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 3}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 1}
    , PB.Slot{PB.color = Green, PB.dice = Dice 2}
    --Row -2
    , PB.Slot{PB.color = Brown, PB.dice = Dice 6}
    , PB.Slot{PB.color = Silver, PB.dice = Dice 1}
    , PB.Slot{PB.color = Yellow, PB.dice = Dice 2}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 5}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 6}
    --Row -3
    , PB.Slot{PB.color = Silver, PB.dice = Dice 3}
    , PB.Slot{PB.color = Yellow, PB.dice = Dice 4}
    , PB.Slot{PB.color = Yellow, PB.dice = Dice 1}
    , PB.Slot{PB.color = Brown, PB.dice = Dice 3}
    ]

standardPlayerBoard :: PB.PlayerBoard
standardPlayerBoard = PB.build PB.blank
    [ PB.setLayout $ zip sortedRange standardSlots
    ]
