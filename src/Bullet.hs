{-# LANGUAGE DeriveGeneric #-}

module Bullet
(
    Bullet(..),
    initBullet
) where

import Types
import Ship

<<<<<<< 55f2461b5b33552c6d92867d2be011fc7b4f9a48
=======
import Graphics.Gloss
import Data.Aeson
import GHC.Generics (Generic)

>>>>>>> Added WebSockets

bulletSpeed :: Float
bulletSpeed = 300

data Bullet = Bullet {
    bulLoc :: Position,
    bulAng :: Degree,
    bulRad :: Radius,
    bulVel :: Speed,
    bulAlive :: Bool
} deriving (Show, Eq, Generic)

instance ToJSON Bullet
instance FromJSON Bullet



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
