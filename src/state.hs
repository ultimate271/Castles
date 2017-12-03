module State
    ( State
    , new
    , depots
    , pbrange
    , hexes
    ) where

import Enum
import qualified Config as CFG
import qualified MainBoard as MB
import qualified PlayerBoard as PB
import qualified Hex (Hex, range, center)

data State = State
    { mainBoard     :: MB.MainBoard
    , playerBoards  :: Player -> PB.PlayerBoard
    , bank          :: Depot -> [HexTile]
    , discard       :: [HexTile]
    , shipmentTrack :: [ShippingTile]
    , players       :: [Player]
    , config        :: CFG.Config
    }

blank :: State
blank = State
    { mainBoard     = MB.new
    , playerBoards  = \_ -> PB.new
    , bank          = \d -> []
    , discard       = []
    , shipmentTrack = []
    , players       = []
    , config        = CFG.new
    }

test :: State
test = State
    { mainBoard     = MB.MainBoard { MB.market = \_ -> [Castle] }
    , playerBoards  = \_ -> PB.incStorage PB.new Mine
    , bank          = \_ -> [Port]
    , discard       = []
    , shipmentTrack = []
    , players       = [Player "Brett" 1, Player "Kyle" 2]
    , config        = CFG.new
    }

new :: CFG.Config -> State
new cfg = blank {config = cfg}

type Distribute = [HexTile] -> (Depot -> [HexTile])
-- ^Distributes a hexlist to a depot -> hexlist function

type Disperse   = [ShippingTile] -> [ShippingTile]
-- ^Disperses a [ShippingTile] -> [ShippingTile]

fillBank :: :: State -> (Distribute, Disperse) -> ([HexTile], [ShippingTile]) -> State
-- ^Returns a state where the bank and shipment track have been added according
-- to Distribute and Disperse
fillBank s (hl, sl) (hs, ss) = s { bank = hl hs , shipmentTrack = sl ss }

depots :: State -> [Depot]
-- ^Returns a list of all depots
depots s = BlackDepot:[Depot $ Dice i | i <- [1..d]]
    where d = CFG.diceSize $ config s

pbrange :: State -> [Hex.Hex]
-- ^Returns the domain of the player board
pbrange s = Hex.range Hex.center (CFG.hexRadius $ config s)

hexes :: State -> [HexTile]
-- ^Returns an unordered list of all the hex tiles in the state
-- The content of this list should remain constant under "safe" operations
hexes s = dhs ++ bhs ++ mbhs ++ pbhs -- concat ls
    where
        dhs = discard s
        bhs = depots s >>= bank s
        mbhs = MB.hexes (depots s) (mainBoard s)
        pbhs = pbs >>= PB.hexes (pbrange s)
        pbs = map (playerBoards s) (players s)



--PlayerBoard -> [Depot] -> [HexTile]




