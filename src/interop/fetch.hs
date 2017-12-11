module Interop.Fetch where

import Core.Action

import Data.Aeson as JSON

interpret :: JSON.Value -> Action
-- ^Takes a JSON object and translates it into an action
-- TODO implement this
interpret j = ServerAction LoadRound

