{-# LANGUAGE MultiParamTypeClasses #-}

module Enum.Colorable
    ( Colorable (getColor)
    ) where
-- ^
-- Import Enum.Enum

-- | Any enumeration c (the colors) can be interpreted as colors.
-- instance Colorable c a means that we can assign every a to some enumeration of type c
class Enum c => Colorable c a where
    getColor :: a -> c
