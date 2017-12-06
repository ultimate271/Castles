module Processing where

import State
import Action

doAction :: Action -> State -> State
--TODO Make this not the identity function

--Player name and id

--p:player
--a:diceAction
--h:hex
--t:hextile
--g:goodstile
doAction (PlayerAction (Draw p a t)) s
    = s
doAction (PlayerAction (Place p a h t)) s
    = s
doAction (PlayerAction (Ship g t)) s
    = s
doAction (PlayerAction (Buy p a)) s
    = s
doAction (PlayerAction (Purchase p t)) s
    = s
doAction (ServerAction (FillBank ts)) s
    = s
doAction (ServerAction (AssignDice ds)) s
    = s
doAction (ClearRound) s
    = s
doAction (LoadRound) s
    = s
doAction (DoEndgameScoring) s
    = s
