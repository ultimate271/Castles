module Processing where

import State
import PlayerAction

doAction :: State -> PlayerAction -> State
--TODO Make this not the identity function
doAction s a = s
