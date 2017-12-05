module State
    ( State
    --Builders
    , blank
    , addConfig
    , addPlayer
    , fillBank
    , addMainBoard
    , addPlayerBoard
    , setTurnOrder
    --Retrievers
    , playerBoardList
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
    , turnOrder     :: Player -> TurnOrder
    } deriving (Eq, Show)

--Build
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Unsafe
blank :: State
blank = State
    { mainBoard     = MB.new
    , playerBoards  = \_ -> PB.new
    , bank          = \d -> []
    , discard       = []
    , shipmentTrack = []
    , players       = []
    , config        = CFG.new
    , turnOrder     = \_ -> TurnOrder 0 0
    }

addConfig :: State -> Config -> State
addConfig s c = s{config = c}

addPlayer :: State -> Player -> State
addPlayer (s{players = ps}) p = s{players = p:ps}

fillBank :: State -> (Distribute, Disperse) -> ([HexTile], [ShippingTile]) -> State
-- ^Returns a state where the bank and shipment track have been added according
-- to Distribute and Disperse
fillBank s (hl, sl) (hs, ss) = s { bank = hl s hs , shipmentTrack = sl s ss }

addMainBoard :: State -> MainBoard -> State
addMainBoard s m = s{mainBoard = m}

addPlayerBoard :: State -> Player -> PlayerBoard -> State
addPlayerBoard (s{playerBoards = pb'}) p pb =
    s {playerBoards = \p' -> if p == p' then pb else pb' p'}

setTurnOrder :: State -> Player -> TurnOrder -> State
setTurnOrder (s{turnOrder = to'}) p to =
    s {turnOrder = \p' -> if p == p' then to else to' p'}

playerBoardList :: State -> [PB.PlayerBoard]
playerBoardList s = map (playerBoards s) (players s)

type Distribute = State -> [HexTile] -> (Depot -> [HexTile])
-- ^Distributes a hexlist to a depot -> hexlist function

type Disperse   = State -> [ShippingTile] -> [ShippingTile]
-- ^Disperses a [ShippingTile] -> [ShippingTile]

--Retrieve
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

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


