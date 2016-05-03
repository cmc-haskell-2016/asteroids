module GraphObject where

import Types

import Graphics.Gloss


class GraphObject a where
    draw :: a -> Picture
    move :: Time -> a -> a
