module Bullet
(
    Bullet(..),
    initBullet,
    bulletSpeed
) where

import Types
import Ship


bulletSpeed :: Float
bulletSpeed = 300


data Bullet = Bullet {
    bulLoc :: Position,
    bulRad :: Radius,
    bulVel :: Speed,
    bulAlive :: Bool
} deriving (Show, Eq)



initBullet :: Ship -> Bullet
initBullet s =
    Bullet {
        bulLoc = pos,
        bulRad = 3,
        bulAlive = True,
        bulVel = velang
    }
    where
        (x, y) = shipLoc s
        yvel = cos ((shipAng s) * pi / 180)
        xvel = sin ((shipAng s) * pi / 180)
        norm = sqrt (xvel * xvel + yvel * yvel)
        velang = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)
        pos = (x + xvel /norm * 40, y + yvel /norm * 40)