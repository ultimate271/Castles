module Hex
    ( Hex
    , adjacent
    , isAdjacent
    ) where

data Hex = Hex Int Int Int
    deriving (Show, Eq)

adjacent :: Hex -> [Hex]
adjacent (Hex i j k) =
    [ Hex (i+1) (j-1) (k+0)
    , Hex (i+1) (j+0) (k-1)
    , Hex (i+0) (j+1) (k-1)
    , Hex (i+0) (j-1) (k+1)
    , Hex (i-1) (j+1) (k+0)
    , Hex (i-1) (j+0) (k+1)
    ]

isAdjacent :: Hex -> Hex -> Bool
isAdjacent h i = elem h (adjacent i)

distance :: Hex -> Hex -> Int
distance (Hex i j k) (Hex i' j' k') =
    maximum [abs (i - i'), abs (j - j'), abs (k - k')]

range :: Hex -> Int -> [Hex]
range (Hex i j k) r =
    [Hex a b c
    | a <- [i-r..i+r]
    , b <- [j-r..j+r]
    , c <- [k-r..k+r]
    , a + b + c == 0
    ]
