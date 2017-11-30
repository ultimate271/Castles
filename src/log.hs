module Log
    ( Log
    , LogEntry (..)
    , new
    , add
    , retrieve
    ) where

import Data.Time.Clock

data LogEntry a = LogEntry
    { time :: UTCTime
    , value :: a
    , message :: String
    } deriving (Show, Eq)

data Log a = Log [LogEntry a]

new :: Log a
new = Log []

add :: Log a -> LogEntry a -> Log a
add (Log les) le = Log (le:les)

retrieve :: Log a -> UTCTime -> Maybe (LogEntry a)
retrieve (Log []) t = Nothing
retrieve (Log (le:les)) t =
    if time le == t then Just le else retrieve (Log les) t
