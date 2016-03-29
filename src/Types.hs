module Types where

import Graphics.Gloss

---------------------------------------------
--Types

type Radius = Float
type Position = (Float, Float)
type Degree = Float
type Speed = (Float, Float)
type ShipSpeed = Float
type Time = Float
type Step = Int
type Unit = Int

--------------------------------------------
-- Constants

maxShieldTime:: Int
maxShieldTime = 5

mainShipSize:: Float
mainShipSize = 20

speedShip :: Float
speedShip = 100

bulletSize :: Float
bulletSize = 3

shieldRad :: Float
shieldRad = 40

width :: Int
width = 700

height :: Int
height = 500

offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 100

background :: Color
background = makeColorI 25 25 112 0

fps :: Int
fps = 60

bulletSpeed :: Float
bulletSpeed = 200

shipColor :: Color
shipColor = light (light red)

data GameState = Game Step Ship [Asteroid] [Bullet] deriving (Show, Eq)

data Ship = Ship {
    shipLoc :: Position,
    shipVel :: ShipSpeed,
    shipAng :: Degree,
    rotation :: Degree,
    shipAlive :: Bool,
<<<<<<< HEAD
    shipAccel :: Bool
=======
    shieldOn :: Bool,
    shieldAcc:: Unit
>>>>>>> 2d0dde199b47c3c54b0eab57ef777d9ac9d85fe2
} deriving (Show, Eq)

data Asteroid = Asteroid {
    astLoc :: Position,
    astAng :: Degree,
    astSize :: Radius,
    astAlive :: Bool,
    astVel :: Speed
} deriving (Show, Eq)

data Bullet = Bullet {
    bulLoc :: Position,
    bulAng :: Degree,
    bulVel :: Speed,
    bulAlive :: Bool
} deriving (Show, Eq)

