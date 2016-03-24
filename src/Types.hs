module Types where

import Graphics.Gloss

---------------------------------------------
--Types

type Radius = Float
type Position = (Float, Float)
type Degree = Float
type Speed = (Float, Float)
type Time = Float
type Step = Int

--------------------------------------------
-- Constants

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
bulletSpeed = 100

shipColor :: Color
shipColor = light (light red)

data GameState = Game Step Ship [Asteroid] [Bullet] deriving (Show, Eq)

data Ship = Ship {
	shipLoc :: Position,
	shipVel :: Speed,
	shipAng :: Degree,
	shipAlive :: Bool
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

