module Processing where

import State
import Action

doAction :: State -> Action -> State
--TODO Make this not the identity function

--Player name and id

--p:player
--d:dice
--h:hex
--t:hextile
doAction s (PlayerAction (Draw p d t))
    = s
doAction s (PlayerAction (Place p d h t))
    = s
doAction s (PlayerAction (Ship p t))
    = s
doAction s (PlayerAction (Buy p d))
    = s
doAction s (PlayerAction (Purchase p t))
    = s
doAction s (ServerAction (FillBank ts))
    = s
doAction s (ServerAction (AssignDice ds))
    = s
doAction s (ClearRound)
    = s
doAction s (LoadRound)
    = s
doAction s (DoEndgameScoring)
    = s
