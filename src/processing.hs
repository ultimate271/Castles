module Processing where

import State
import Action

doAction :: Action -> State -> State
--TODO Make this not the identity function

--Player name and id

--p:player
--d:dice
--h:hex
--t:hextile
--g:goodstile
doAction (PlayerAction (Draw p d t)) s
    = s
doAction (PlayerAction (Place p d h t)) s
    = s
doAction (PlayerAction (Ship g t)) s
    = s
doAction (PlayerAction (Buy p d)) s
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
