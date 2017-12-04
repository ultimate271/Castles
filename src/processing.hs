module Processing where

import State
import Action

doAction :: State -> Action -> State
--TODO Make this not the identity function

--Player name and id

doAction s (PlayerAction (Draw p d h))
    = s
doAction s (PlayerAction (Place p d h t))
    = s
doAction s (PlayerAction (Ship p h))
    = s
doAction s (PlayerAction (Buy p d))
    = s
doAction s (PlayerAction (Purchase p h))
    = s
doAction s (ServerAction (FillBank hs))
    = s
doAction s (ServerAction (AssignDice ds))
    = s
doAction s (ClearRound)
    = s
doAction s (LoadRound)
    = s
doAction s (DoEndgameScoring)
    = s
