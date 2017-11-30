module Hex
    ( Hex (Axial)
    , center
    , adjacent
    , isAdjacent
    , range
    ) where

data Hex = Cube Int Int Int | Axial Int Int
    deriving (Show)

instance Eq Hex where
    (==) (Cube i j k) (Cube i' j' k') = i==i' && j==j' && k==k'
    (==) h i = (toCube h) == (toCube i)

center :: Hex
center = Cube 0 0 0

convert :: Hex -> Hex
convert (Cube i j _) = Axial i j
convert (Axial i j) = Cube i j (-i-j)

toCube :: Hex -> Hex
toCube h@(Cube _ _ _) = h
toCube h@(Axial _ _) = convert h

toAxial :: Hex -> Hex
toAxial h@(Cube _ _ _) = convert h
toAxial h@(Axial _ _) = h

adjacent :: Hex -> [Hex]
adjacent (Cube i j k) =
    [ Cube (i+1) (j-1) (k+0)
    , Cube (i+1) (j+0) (k-1)
    , Cube (i+0) (j+1) (k-1)
    , Cube (i+0) (j-1) (k+1)
    , Cube (i-1) (j+1) (k+0)
    , Cube (i-1) (j+0) (k+1)
    ]
adjacent h@(Axial _ _) = map toAxial $ adjacent (toCube h)

isAdjacent :: Hex -> Hex -> Bool
isAdjacent h i = elem h (adjacent i)

distance :: Hex -> Hex -> Int
distance (Cube i j k) (Cube i' j' k') =
    maximum [abs (i - i'), abs (j - j'), abs (k - k')]
distance h i = distance (toCube h) (toCube i)

range :: Hex -> Int -> [Hex]
range (Cube i j k) r =
    [Cube a b c
    | a <- [i-r..i+r]
    , b <- [j-r..j+r]
    , c <- [k-r..k+r]
    , a + b + c == 0
    ]
range h@(Axial _ _) r = map toAxial $ range (toCube h) r
