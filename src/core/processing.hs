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
    = s
doAction (PlayerAction (Place p a h t)) s
    = s
doAction (PlayerAction (Ship g t)) s
    = s
doAction (PlayerAction (Buy p a)) s
    = s
doAction (PlayerAction (Purchase p t)) s
    = s
doAction (PlayerAction (DrawGoods p d gs)) s
    = s
doAction (ServerAction (FillBank ts)) s
    = s
doAction (ServerAction (AssignDice ds)) s
    = s
doAction (ServerAction (ClearRound)) s
    = s
doAction (ServerAction (LoadRound)) s
    = s
doAction (ServerAction (DoEndgameScoring)) s
    = s
