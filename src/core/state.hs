module Core.State
    ( State
    , Player (..)
    , TurnOrder (..)
    , StateError (..)
    , Phase (..)
    , GameState (..)
    , kill
    --Builders
    , blank
    , addConfig
    , addPlayer
    , addToBank
    , addMainBoard
    , addPlayerBoard
    , addBonusTile
    , removeBonusTile
    , setTurnOrder
    --Retrievers
    , pbList
    , depots
    , pbrange
    , allHexes
    , allGoods
    , phaseNumber
    , turnNumber
    --Pass Through
    , StateError (..)
    , Phase (..)
    , GameState (..)
    ) where

import Enum.Enum
import qualified Enum.Config as CFG
import Enum.Hex (Hex)
import qualified Enum.Hex as Hex (range, center)
import qualified Core.MainBoard as MB
import qualified Core.PlayerBoard as PB

import Data.List (sortBy, delete)

data Player = Player
    { name :: String
    , id :: Int
    } deriving (Show, Eq)
data TurnOrder = TurnOrder Int Int
    deriving (Show, Eq)
instance Ord TurnOrder where
    (<=) (TurnOrder i j) (TurnOrder i' j') = i <= i' || i == i' && j <= j'
data StateError
    = StorageFull Player
    | HexTaken Player Hex
    | IllegalAction Player
    deriving (Eq, Show)
data Phase
    = Setup
    | Phase Int
    | GameEnd
    deriving (Eq, Show)
data GameState
    = GameState
        { playerQueue :: [Player]
        }
    | StateError StateError
    deriving (Eq, Show)

data State = State
    { mainBoard      :: MB.MainBoard
    , playerBoards   :: Player -> PB.PlayerBoard
    , bank           :: [(MB.Slot, [HexTile])]
    , discard        :: [HexTile]
    , shipmentTrack  :: [GoodsTile]
    , players        :: [Player]
    , config         :: CFG.Config
    , turnOrderTrack :: Player -> TurnOrder
    , gameState      :: GameState
    , bonusTiles     :: [BonusTile]
    }
instance Show State where
    show s = "State "
        ++ "{ mainBoard = "      ++ MB.toString (depots s) (mainBoard s)
        ++ ", playerBoards = "   ++ showPlayerBoards
        ++ ", bank = "           ++ (show $ bank s)
        ++ ", discard = "        ++ (show $ discard s)
        ++ ", shipmentTrack = "  ++ (show $ shipmentTrack s)
        ++ ", players = "        ++ (show $ players s)
        ++ ", config = "         ++ (show $ config s)
        ++ ", turnOrderTrack = " ++ showTurnOrder
        ++ ", gameState "        ++ (show $ gameState s)
        ++ ", bonusTiles "       ++ (show $ bonusTiles s)
        ++ "}"
      where show1PlayerBoard p =
                show p ++ " -> " ++ PB.toString (pbrange s) (playerBoards s p)
            showPlayerBoards =
                foldr (\p acc -> show1PlayerBoard p ++ " | " ++ acc) "" (players s)
            --show1Bank d =
            --    show d ++ " -> " ++ show (bank s d)
            --showBank =
            --    foldr (\d acc -> show1Bank d ++ " | " ++ acc) "" (depots s)
            show1TurnOrder p =
                show p ++ " -> " ++ show (turnOrderTrack s p)
            showTurnOrder =
                foldr (\d acc -> show1TurnOrder d ++ " | " ++ acc) "" (players s)

--Build (Unsafe)----------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

blank :: State
blank = State
    { mainBoard      = MB.blank
    , playerBoards   = \_ -> PB.blank
    , bank           = []
    , discard        = []
    , shipmentTrack  = []
    , players        = []
    , config         = CFG.blank
    , turnOrderTrack = \_ -> TurnOrder 0 0
    , gameState      = GameState{playerQueue = []}
    , bonusTiles     = []
    }

addConfig :: CFG.Config -> State-> State
-- ^Returns a state that is just like s but with config c
addConfig c s = s{config = c}

addPlayer :: Player -> State -> State
-- ^Returns a state that is just like s but with p added to players
addPlayer p s@State{players = ps} = s{players = p:ps}

addToBank :: [(MB.Slot, [HexTile])] -> State -> State
-- ^Adds the HexTiles to the bank
-- Note that this does not add hextiles to an existing slot, this creates a new
-- slot on the board with new hextiles in it.
addToBank ts s@State{bank = b} = s{bank = b ++ ts}

--fillBank :: ([HexTile], [GoodsTile]) -> (Distribute, Disperse) -> State -> State
-- ^Returns a state where the bank and shipment track have been added according
-- to Distribute and Disperse
--fillBank (hs, ss) (hl, sl) s = s { bank = hl s hs , shipmentTrack = sl s ss }

addMainBoard :: MB.MainBoard -> State -> State
addMainBoard m s = s{mainBoard = m}

addPlayerBoard :: Player -> PB.PlayerBoard -> State -> State
addPlayerBoard p pb s@State{playerBoards = pb'} =
    s {playerBoards = \p' -> if p' == p then pb else pb' p'}

setTurnOrder :: Player -> TurnOrder -> State -> State
setTurnOrder p to s@State{turnOrderTrack = to'} =
    s {turnOrderTrack = \p' -> if p' == p then to else to' p'}

addBonusTile :: BonusTile -> State -> State
addBonusTile b s@State{bonusTiles = bs} = s { bonusTiles = b:bs }

removeBonusTile :: BonusTile -> State -> State
removeBonusTile b s@State{bonusTiles = bs} = s { bonusTiles = delete b bs }

build :: State -> [State -> State] -> State
build = foldr (\f s -> f s)

kill :: StateError -> State -> State
kill e s = s {gameState = StateError e}

--Retrieve----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

pbList :: State -> [PB.PlayerBoard]
-- ^Returns the list of PlayerBoards
pbList s = (playerBoards s) <$> (players s)

depots :: State -> [MB.Depot]
-- ^Returns a list of all depots
depots State{config = CFG.Config{CFG.diceSize = d}} =
    MB.BlackDepot:[MB.Depot $ Dice i | i <- [1..d]]

pbrange :: State -> [Hex]
-- ^Returns the domain of the player board
pbrange s = Hex.range Hex.center (CFG.hexRadius $ config s)

allHexes :: State -> [HexTile]
-- ^Returns an unordered list of all the hex tiles in the state
-- The content of this list should remain constant under "safe" operations
allHexes s = dhs ++ bhs ++ mbhs ++ pbhs -- concat ls
  where dhs = discard s
        bhs = bank s >>= snd
        mbhs = MB.allHexes (depots s) (mainBoard s)
        pbhs = pbList s >>= PB.allHexes (pbrange s)

allGoods :: State -> [GoodsTile]
-- ^Returns an unordered list of all the goods tiles in the state
-- The content of this list should remain constant under "safe" operations
allGoods s = mbgoods ++ pbgoods ++ shipmentTrack s
  where mbgoods = MB.allGoods (depots s) (mainBoard s)
        pbgoods = pbList s >>= PB.allGoods

turnOrderList :: State -> [Player]
-- ^Returns the list of players
turnOrderList State{turnOrderTrack = to, players = ps} =
    sortBy (\p1 p2 -> compare (to p2) (to p1)) ps

phaseNumber :: State -> Phase
-- ^Finds the current phase based on the shipping track
phaseNumber State{shipmentTrack = gs, config = c, gameState = g} =
    if l == p * t then Setup
    else if l == 0 && length ps == 0 then GameEnd
    else Phase $ p - div l t
  where ps = playerQueue g
        l = length gs
        p = CFG.phaseCount c
        t = CFG.turnsPerPhase c

turnNumber :: State -> Int
-- ^Finds which turn it is in the phase based on the shipping track
turnNumber State{shipmentTrack = gs, config = c} =
    t - 1 - length gs `mod` t
  where t = CFG.turnsPerPhase c

