module Bullet
(
    Bullet(..),
    initBullet
) where

import Types
import Ship
import GraphObject

import Graphics.Gloss

bulletSpeed :: Float
bulletSpeed = 200


data Bullet = Bullet {
    bulLoc :: Position,
    bulAng :: Degree,
    bulRad :: Radius,
    bulVel :: Speed,
    bulAlive :: Bool
} deriving (Show, Eq)


instance GraphObject Bullet where
    draw bull =
            translate x y $
            color red $
            circleSolid 3
        where
            (x, y) = bulLoc bull

    move sec bull =
        bull {
            bulLoc = (x1, y1)
        }
        where
            (x, y) = bulLoc bull
            (vx, vy) = bulVel bull
            x1 = x + vx * sec
            y1 = y + vy * sec


initBullet :: Ship -> Bullet
initBullet s =
    Bullet {
        bulLoc = shipLoc s,
        bulRad = 3,
        bulAng = shipAng s,
        bulAlive = True,
        bulVel = velang
    }
    where
        yvel = cos ((shipAng s) * pi / 180)
        xvel = sin ((shipAng s) * pi / 180)
        norm = sqrt (xvel * xvel + yvel * yvel)
        velang = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)
