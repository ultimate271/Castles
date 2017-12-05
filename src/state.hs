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
    , shipmentTrack :: [GoodsTile]
    , players       :: [Player]
    , config        :: CFG.Config
    , turnOrder     :: Player -> TurnOrder
    }
instance Show State where
    show s = " State "
        ++ "{ mainBoard = " ++ MB.toString (depots s) (mainBoard s)
        ++ ", playerBoards = " ++ showPlayerBoards
        ++ ", bank = " ++ showBank
        ++ ", discard = " ++ (show $ discard s)
        ++ ", shipmentTrack = " ++ (show $ shipmentTrack s)
        ++ ", players = " ++ (show $ players s)
        ++ ", config = " ++ (show $ config s)
        ++ ", turnOrder " ++ showTurnOrder
      where show1PlayerBoard p =
                show p ++ " -> " ++ PB.toString (pbrange s) (playerBoards s p)
            showPlayerBoards =
                foldr (\p acc -> acc ++ " | " ++ show1PlayerBoard p) "" (players s)
            show1Bank d =
                show d ++ " -> " ++ show (bank s d)
            showBank =
                foldr (\d acc -> acc ++ " | " ++ show1Bank d) "" (depots s)
            show1TurnOrder p =
                show p ++ " -> " ++ show (turnOrder s p)
            showTurnOrder =
                foldr (\d acc -> acc ++ " | " ++ show1TurnOrder d) "" (players s)


type Distribute = State -> [HexTile] -> (Depot -> [HexTile])
-- ^Distributes a hexlist to a depot -> hexlist function

type Disperse   = State -> [GoodsTile] -> [GoodsTile]
-- ^Disperses a [GoodsTile] -> [GoodsTile]

--Build (Unsafe)----------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: State
blank = State
    { mainBoard     = MB.blank
    , playerBoards  = \_ -> PB.blank
    , bank          = \d -> []
    , discard       = []
    , shipmentTrack = []
    , players       = []
    , config        = CFG.new
    , turnOrder     = \_ -> TurnOrder 0 0
    }

addConfig :: CFG.Config -> State-> State
-- ^Returns a state that is just like s but with config c
addConfig c s = s{config = c}

addPlayer :: Player -> State -> State
-- ^Returns a state that is just like s but with p added to players
addPlayer p s@State{players = ps} = s{players = p:ps}

fillBank :: ([HexTile], [GoodsTile]) -> (Distribute, Disperse) -> State -> State
-- ^Returns a state where the bank and shipment track have been added according
-- to Distribute and Disperse
fillBank (hs, ss) (hl, sl) s = s { bank = hl s hs , shipmentTrack = sl s ss }

addMainBoard :: MB.MainBoard -> State -> State
addMainBoard m s = s{mainBoard = m}

addPlayerBoard :: Player -> PB.PlayerBoard -> State -> State
addPlayerBoard p pb s@State{playerBoards = pb'} =
    s {playerBoards = \p' -> if p == p' then pb else pb' p'}

setTurnOrder :: Player -> TurnOrder -> State -> State
setTurnOrder p to s@State{turnOrder = to'} =
    s {turnOrder = \p' -> if p == p' then to else to' p'}

--Retrieve----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

playerBoardList :: State -> [PB.PlayerBoard]
-- ^Returns the list of PlayerBoards
playerBoardList s = map (playerBoards s) (players s)

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


