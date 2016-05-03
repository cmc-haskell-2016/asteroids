module Asteroid
(
    Asteroid(..)
) where

import Types
import GraphObject

import Graphics.Gloss


data Asteroid = Asteroid {
    astLoc :: Position,
    astAng :: Degree,
    astRad :: Radius,
    astVel :: Speed,
    astAlive :: Bool
} deriving (Show, Eq)


instance GraphObject Asteroid where
    draw ast =
        translate x y $
        color black $
        circleSolid rad
        where
            (x, y) = astLoc ast
            rad = astRad ast

    move sec ast =
        ast {
            astLoc = (x1, y1)
        }
        where
            (x, y) = astLoc ast
            (vx, vy) = astVel ast
            x1 = x + vx * sec
            y1 = y + vy * sec
