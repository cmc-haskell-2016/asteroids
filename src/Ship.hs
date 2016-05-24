{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Ship
(
    Ship(..),
    accelerate,
    initShip,
    shipColors,
    updateShip
) where

import Types
import Collisions

import Graphics.Gloss.Rendering
import Graphics.Gloss
import Data.Aeson
import GHC.Generics (Generic)
import Network.WebSockets
import qualified Data.ByteString.Lazy.Char8 as BL8


shipColors :: Int -> Color
shipColors n
    | n == 1 = light $ light red
    | n == 2 = light $ light yellow
    | otherwise = light $ light green

maxShieldPower:: Int
maxShieldPower = 70

maxShipSpeed::ShipSpeed
maxShipSpeed = 100


data Ship = Ship {
    shipSize :: Radius,
    shipLoc :: Position,
    shipVel :: ShipSpeed,
    shipAng :: Degree,
    rotation :: Degree,
    shipAlive :: Bool,
    shipAccel :: Bool,
    shipColor :: Int,
    shieldOn :: Bool,
    shieldAcc:: Unit,
    shieldRad :: Float
} deriving (Show, Eq, Read, Generic)

instance ToJSON Ship
instance FromJSON Ship

instance WebSocketsData Ship where
    fromLazyByteString = read . BL8.unpack
    toLazyByteString   = BL8.pack . show


accelerate :: Float -> Ship -> Ship
accelerate sec s
    | shipAccel s =
        if shipVel s + 3 < maxShipSpeed
            then newShip {shipVel = shipVel s + 3 }
            else newShip {shipVel = maxShipSpeed}
    | otherwise = newShip {shipVel = 0.98 * (shipVel s)}
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


initShip :: Int -> Ship
initShip n = Ship {
    shipSize = 20,
    shipLoc = (-(fromIntegral width) / 2 + (fromIntegral (width * n)) / 3, 0),
    shipAng = 0,
    rotation = 0,
    shipVel = 0,
    shipAlive = True,
    shipAccel = False,
    shipColor = n,
    shieldOn = False,
    shieldAcc = maxShieldPower,
    shieldRad = 40
}
