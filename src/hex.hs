module Hex where

data Hex = Hex Int Int
    deriving (Show, Eq)

adjacent :: Hex -> [Hex]
adjacent (Hex i j) =
    [ Hex (i+2) j
    , Hex (i-2) j
    , Hex (i+1) (j+1)
    , Hex (i-1) (j+1)
    , Hex (i+1) (j-1)
    , Hex (i-1) (j-1)
    ]

isAdjacent :: Hex -> Hex -> Bool
isAdjacent h i = elem h (adjacent i)
