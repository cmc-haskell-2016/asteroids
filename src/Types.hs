module Types where

import Graphics.Gloss

---------------------------------------------
--Types 

type Radius = Float
type Position = (Float, Float)
type Degree = Float
type Speed = (Float, Float)
type Time = Float

--------------------------------------------
-- Constants

width :: Int
width = 700

height :: Int
height = 500

offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 250

fps :: Int
fps = 60

bulletSpeed :: Int
bulletSpeed = 100

shipColor :: Color
shipColor = light (light red)

data GameState = Game Ship [Asteroid] [Bullet]

data Ship = Ship {
	shipLoc :: Position,
	shipVel :: Speed,
	shipAng :: Degree,
	shipAlive :: Bool
} deriving Show

data Asteroid = Asteroid {
	astLoc :: Position,
	astAng :: Degree,
	astSize :: Radius,
	astAlive :: Bool
} deriving Show

data Bullet = Bullet {
	bulLoc :: Position,
	bulAng :: Degree,
	bulAlive :: Bool
} deriving Show

