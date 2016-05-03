module GraphObjects where

import Types

import Graphics.Gloss.Rendering
import Graphics.Gloss


data Ship = Ship {
    shipSize :: Radius,
    shipLoc :: Position,
    shipVel :: ShipSpeed,
    shipAng :: Degree,
    rotation :: Degree,
    shipAlive :: Bool,
    shipAccel :: Bool,
    shieldOn :: Bool,
    shieldAcc:: Unit,
    shieldRad :: Float
} deriving (Show, Eq)

data Asteroid = Asteroid {
    astLoc :: Position,
    astAng :: Degree,
    astSize :: Radius,
    astVel :: Speed,
    astAlive :: Bool
} deriving (Show, Eq)

data Bullet = Bullet {
    bulLoc :: Position,
    bulAng :: Degree,
    bulSize :: Radius,
    bulVel :: Speed,
    bulAlive :: Bool
} deriving (Show, Eq)


shipColor :: Color
shipColor = light (light red)


class GraphObject a where
    draw :: a -> Picture


instance GraphObject Asteroid where
    draw ast =
        translate x y $
        color black $
        circleSolid rad
        where
            (x, y) = astLoc ast
            rad = astSize ast


instance GraphObject Bullet where
    draw bull =
            translate x y $
            color red $
            circleSolid 3
        where
            (x, y) = bulLoc bull


instance GraphObject Ship where
    draw s =
        if (shipAlive s)
        then
            pictures[
                translate x y $
                color shipColor $
                rotate phi $
                polygon [(10, -5), (0, 0), (0, (shipSize s))],
                translate x y $
                color shipColor $
                rotate phi $
                polygon [(-10, -5), (0, 0), (0, (shipSize s))]]
        else
            scale 10 10 (pictures[
                translate x y $
                color shipColor $
                rotate phi $
                polygon [(10, -5), (0, 0), (0, (shipSize s))],
                translate x y $
                color shipColor $
                rotate phi $
                polygon [(-10, -5), (0, 0), (0, (shipSize s))],
                translate (-18) (-20) $
                scale 0.05 0.05 $
                color red $
                text "Game Over"])
        where
            (x, y) = shipLoc s
            phi = shipAng s
