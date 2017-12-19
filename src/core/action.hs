module Core.Action
    ( PlayerAction (..)
    , ServerAction (..)
    , Action(doAction)
    ) where

import Enum.Enum
import Enum.Hex
import qualified Core.PlayerBoard as PB
import qualified Core.MainBoard as MB
import qualified Core.State as S

class Action a where
    doAction :: a -> S.State -> S.State

data PlayerAction
    --Draw a hex from the main board and put it in your storage area
    = Draw S.Player PB.DiceAction MB.Depot HexTile
    --Place a hex from the storage area onto your player layout
    | Place S.Player PB.DiceAction Hex HexTile
    --Ship goods from the dock
    | Ship S.Player PB.DiceAction GoodsTile
    --Trade die for worker tiles
    | TradeDie S.Player PB.DiceAction
    --Purchase hextile using silverlings
    | Purchase S.Player MB.Depot HexTile
    --Discard a hextile from the storage area
    | Discard S.Player HexTile
    --Draw goods from a main board depot and put into your dock
    | DrawGoods S.Player MB.Depot [GoodsTile]
    --End your turn at this stage. Leftover dice will be traded for workers
    | Finish S.Player
    deriving (Eq, Show)
instance Action PlayerAction where
    doAction (Draw p a d t) s
    -- ^p draws t from mainboard depot d using diceaction a
        = s
    doAction (Place p a h t) s
    -- ^p places t onto playerboard hex h using diceaction a
        = s
    doAction (Ship p a g) s
    -- ^p ships all goods g on their playerboard dock using diceaction a
        = s
    doAction (TradeDie p a) s
    -- ^p uses diceaction a to buy workers
        = s
    doAction (Purchase p d t) s
    -- ^p uses silverlings to purchase t from the mainboard
        = s
    doAction (Discard p t) s
    -- ^p discards t from their storage area to make room for more tiles
        = s
    doAction (DrawGoods p d gs) s
    -- ^p draws gs from mainboard depot d after building a boat
        = s
data ServerAction
    = DoSetup
        { bank :: [HexTile]
        , track :: [GoodsTile]
        , players :: [S.Player] --Order matters here
        }
    | LoadTurn
        { whiteDice  :: Dice
        , playerDice :: [(S.Player, Dice)]
        }
    | LoadRound
    deriving (Eq, Show)
instance Action ServerAction where
    doAction a@DoSetup{} s
    -- ^Setup the game according to the specified setup
        = s
    doAction a@LoadTurn{} s
    -- ^Reorg the board after a turn is completed
        = s
    doAction LoadRound s
    -- ^Reorg the board after a round is completed
        = s
