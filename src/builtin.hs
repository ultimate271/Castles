module Builtin where

import           Enum.Enum
import           Enum.Hex (Hex)
import qualified Enum.Hex         as Hex
import           Core.PlayerBoard (PlayerBoard)
import qualified Core.PlayerBoard as PB
import           Core.MainBoard (MainBoard)
import qualified Core.MainBoard   as MB
import           Core.State (State)
import qualified Core.State       as S
import           Core.Action (Action)
import qualified Core.Action      as A

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
    , CFG.depotCount = 6
    }

standardMainBoard :: Int -> MB.MainBoard
-- ^vanilla main board with n players
standardMainBoard 2 = MB.build MB.blank
    [ MB.addToSlots (MB.Depot $ Dice 1) (Just Blue)
    , MB.addToSlots (MB.Depot $ Dice 1) (Just Brown)   
    , MB.addToSlots (MB.Depot $ Dice 2) (Just Burgundy)
    , MB.addToSlots (MB.Depot $ Dice 2) (Just Yellow)  
    , MB.addToSlots (MB.Depot $ Dice 3) (Just Green)   
    , MB.addToSlots (MB.Depot $ Dice 3) (Just Brown)   
    , MB.addToSlots (MB.Depot $ Dice 4) (Just Blue)    
    , MB.addToSlots (MB.Depot $ Dice 4) (Just Brown)   
    , MB.addToSlots (MB.Depot $ Dice 5) (Just Silver)  
    , MB.addToSlots (MB.Depot $ Dice 5) (Just Yellow)  
    , MB.addToSlots (MB.Depot $ Dice 6) (Just Green)   
    , MB.addToSlots (MB.Depot $ Dice 6) (Just Brown)   
    , MB.addToSlots (MB.BlackDepot)     (Nothing)      
    , MB.addToSlots (MB.BlackDepot)     (Nothing)      
    , MB.addToSlots (MB.BlackDepot)     (Nothing)      
    , MB.addToSlots (MB.BlackDepot)     (Nothing)      
    ]
standardMainBoard 3 = MB.build (standardMainBoard 2)
    [ MB.addToSlots (MB.Depot $ Dice 1) (Just Yellow)   
    , MB.addToSlots (MB.Depot $ Dice 2) (Just Brown)    
    , MB.addToSlots (MB.Depot $ Dice 3) (Just Blue)     
    , MB.addToSlots (MB.Depot $ Dice 4) (Just Green)    
    , MB.addToSlots (MB.Depot $ Dice 5) (Just Brown)    
    , MB.addToSlots (MB.Depot $ Dice 6) (Just Burgundy) 
    , MB.addToSlots (MB.BlackDepot)     (Nothing)       
    , MB.addToSlots (MB.BlackDepot)     (Nothing)       
    ]
standardMainBoard 4 = MB.build (standardMainBoard 3)
    [ MB.addToSlots (MB.Depot $ Dice 1) (Just Green)  
    , MB.addToSlots (MB.Depot $ Dice 2) (Just Brown)  
    , MB.addToSlots (MB.Depot $ Dice 3) (Just Yellow) 
    , MB.addToSlots (MB.Depot $ Dice 4) (Just Silver) 
    , MB.addToSlots (MB.Depot $ Dice 5) (Just Brown)  
    , MB.addToSlots (MB.Depot $ Dice 6) (Just Blue)   
    , MB.addToSlots (MB.BlackDepot)     (Nothing)     
    , MB.addToSlots (MB.BlackDepot)     (Nothing)     
    ]

typicalMainBoard :: MainBoard
typicalMainBoard = MB.build (standardMainBoard 4)
    [ MB.addToMarket (MB.Depot $ Dice 1) Castle
    , MB.addToMarket (MB.Depot $ Dice 1) Boat
    , MB.addToMarket (MB.Depot $ Dice 2) (Pasture Cow 3)
    , MB.addToMarket (MB.Depot $ Dice 2) (Mine)
    , MB.addToMarket (MB.Depot $ Dice 3) (Knowledge Unrestrict)
    , MB.addToMarket (MB.Depot $ Dice 3) (Building Church)
    , MB.addToMarket (MB.Depot $ Dice 3) Castle
    , MB.addToMarket (MB.Depot $ Dice 6) (Building Warehouse)
    , MB.addToMarket MB.BlackDepot Castle
    , MB.addToMarket MB.BlackDepot Boat
    , MB.addToMarket MB.BlackDepot Boat
    , MB.addToMarket MB.BlackDepot Mine
    , MB.addToMarket MB.BlackDepot Mine
    , MB.addToMarket MB.BlackDepot (Pasture Pig 4)
    , MB.addToWarehouse (MB.Depot $ Dice 1) (GoodsTile $ Dice 4)
    , MB.addToWarehouse (MB.Depot $ Dice 1) (GoodsTile $ Dice 2)
    , MB.addToWarehouse (MB.Depot $ Dice 3) (GoodsTile $ Dice 6)
    , MB.addToWarehouse (MB.Depot $ Dice 6) (GoodsTile $ Dice 6)
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
    , PB.Slot{PB.color = Burgundy, PB.dice = Dice 6}
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

typicalPlayerBoard :: PB.PlayerBoard
typicalPlayerBoard = PB.build standardPlayerBoard
    [ PB.incSilverling 3
    , PB.incWorker 2
    , PB.incVictoryTrack 26
    , PB.addDiceAction (PB.Standard $ Dice 4)
    , PB.addDiceAction (PB.Standard $ Dice 2)
    , PB.addDiceAction PB.Buy
    , PB.addToStorage (Pasture Cow 3)
    , PB.addToStorage (Castle)
    , PB.addToDock (GoodsTile $ Dice 3)
    , PB.addToDock (GoodsTile $ Dice 3)
    , PB.addToDock (GoodsTile $ Dice 2)
    , PB.addToShipped (GoodsTile $ Dice 1)
    , PB.addToShipped (GoodsTile $ Dice 6)
    , PB.addToShipped (GoodsTile $ Dice 4)
    , PB.addToShipped (GoodsTile $ Dice 4)
    , PB.placeHex Castle (Hex.Axial 0 0)
    , PB.placeHex Boat (Hex.Axial (-1) 1)
    , PB.placeHex (Pasture Cow 4) (Hex.Axial (-1) 2)
    , PB.placeHex (Building Church) (Hex.Axial 0 1)
    , PB.placeHex (Pasture Pig 3) (Hex.Axial 0 2)
    , PB.placeHex Mine (Hex.Axial (-1) 0)
    ]
