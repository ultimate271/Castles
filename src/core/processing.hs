module Core.Processing where

import Core.State
import Core.Action

validateState :: State -> State -> State
-- ^Function that takes a base state, and a subsequent state, and returns the
-- subsequent state, with a state error if it is invalid.
validateState s t = t

getActions :: State -> [Action]
-- ^Function that takes a state and returns a list of valid actions
-- that can be taken on that state.
getActions s = []

doAction :: Action -> State -> State
-- ^The core of processing, takes an action and a state, and returns the
-- state that is obtained by performing that action on the state.
-- TODO Make this not the identity function
--p:player
--a:diceAction
--d:depot
--h:hex
--t:hextile
--g:goodstile
doAction (PlayerAction (Draw p a d t)) s
-- ^p draws t from mainboard depot d using diceaction a
    = s
doAction (PlayerAction (Place p a h t)) s
-- ^p places t onto playerboard hex h using diceaction a
    = s
doAction (PlayerAction (Ship p a g)) s
-- ^p ships all goods g on their playerboard dock using diceaction a
    = s
doAction (PlayerAction (Buy p a)) s
-- ^p uses diceaction a to buy workers
    = s
doAction (PlayerAction (Purchase p t)) s
-- ^p uses silverlings to purchase t from the mainboard
    = s
doAction (PlayerAction (Discard p t)) s
-- ^p discards t from their storage area to make room for more tiles
    = s
doAction (PlayerAction (DrawGoods p d gs)) s
-- ^p draws gs from mainboard depot d after building a boat
    = s
doAction (ServerAction a@Setup{}) s
-- ^Setup the game according to the specified setup
    = s
doAction (ServerAction a@LoadTurn{}) s
-- ^Reorg the board after a turn is completed
    = s
doAction (ServerAction (LoadRound)) s
-- ^Reorg the board after a round is completed
    = s
