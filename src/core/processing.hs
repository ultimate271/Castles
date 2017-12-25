module Core.Processing
    ( validateState
    , getActions
    , doAction
    ) where
-- ^
-- import           Core.Processing ()
-- -- insert the desired functions in ()

import           Enum.Enum
import           Enum.Hex         (Hex)
import qualified Enum.Hex         as Hex
import           Core.MainBoard   (MainBoard)
import qualified Core.MainBoard   as MB
import           Core.PlayerBoard (PlayerBoard)
import qualified Core.PlayerBoard as PB
import           Enum.Config      (Config)
import qualified Enum.Config      as CFG
import           Core.State       (State)
import qualified Core.State       as S
import           Core.Action      (Action)
import qualified Core.Action      as A

validateState :: State -> State -> State
-- ^Function that takes a base state, and a subsequent state, and returns the
-- subsequent state, with a state error if it is invalid.
validateState s t = t

getActions :: Action a => State -> [a]
-- ^Function that takes a state and returns a list of valid actions
-- that can be taken on that state.
getActions s = []


doAction :: Action a => a -> State -> State
-- ^Function that takes an action and a state, and returns the state
-- that is acquired from taking that action on that state
doAction = A.doAction

-- ^The core of processing, takes an action and a state, and returns the
-- state that is obtained by performing that action on the state.
-- TODO Make this not the identity function
--p:player
--a:diceAction
--d:depot
--h:hex
--t:hextile
--g:goodstile
--doAction (A.PlayerAction (A.Draw p a d t)) s
---- ^p draws t from mainboard depot d using diceaction a
--    = s
--doAction (A.PlayerAction (A.Place p a h t)) s
---- ^p places t onto playerboard hex h using diceaction a
--    = s
--doAction (A.PlayerAction (A.Ship p a g)) s
---- ^p ships all goods g on their playerboard dock using diceaction a
--    = s
--doAction (A.PlayerAction (A.TradeDie p a)) s
---- ^p uses diceaction a to buy workers
--    = s
--doAction (A.PlayerAction (A.Purchase p d t)) s
---- ^p uses silverlings to purchase t from the mainboard
--    = s
--doAction (A.PlayerAction (A.Discard p t)) s
---- ^p discards t from their storage area to make room for more tiles
--    = s
--doAction (A.PlayerAction (A.DrawGoods p d gs)) s
---- ^p draws gs from mainboard depot d after building a boat
--    = s
--doAction (A.ServerAction a@A.DoSetup{}) s
---- ^Setup the game according to the specified setup
--    = s
--doAction (A.ServerAction a@A.LoadTurn{}) s
---- ^Reorg the board after a turn is completed
--    = s
--doAction (A.ServerAction (A.LoadRound)) s
---- ^Reorg the board after a round is completed
--    = s
--doAction _ s = s
