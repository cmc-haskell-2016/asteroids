{-# LANGUAGE RecordWildCards #-}
module Ship
(
    Ship(..),
    accelerate,
    initShip,
    shipColor,
    updateShip
) where

import Types

import Graphics.Gloss.Rendering
import Graphics.Gloss


shipColor :: Color
shipColor = light (light red)

maxShieldPower:: Int
maxShieldPower = 70


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


accelerate :: Float -> Ship -> Ship
accelerate sec s =
    if  shipAccel s
        then newShip {shipVel = shipVel s  + 3 }
        else newShip {shipVel = 0.98 * (shipVel s)
        }
    where
        (x, y) = shipLoc s
        v = shipVel s
        newAng = (shipAng s) + ((rotation s) / 1.5)
        x1 = x + v* (sin (newAng*pi/180)) * sec
        y1 = y + v* (cos (newAng*pi/180)) * sec
        newShip = s {shipAng = newAng, shipLoc = (x1, y1)}


updateShield :: Ship -> Ship
updateShield s@Ship{..} =
    if shieldOn then
        if shieldAcc == 0
            then s {shieldOn = False}
            else s {shieldAcc = shieldAcc - 1}
    else
        if shieldAcc >= maxShieldPower
            then s
            else s {shieldAcc = shieldAcc + 1}


updateShip :: Ship -> Ship
updateShip s = updateShield s


initShip :: Ship
initShip = Ship {
    shipSize = 20,
    shipLoc = (0, 0),
    shipAng = 0,
    rotation = 0,
    shipVel = 0,
    shipAlive = True,
    shipAccel = False,
    shieldOn = False,
    shieldAcc = maxShieldPower,
    shieldRad = 40
}
