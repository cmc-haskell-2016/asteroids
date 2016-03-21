module Types where

import Graphics.Gloss

---------------------------------------------
--Types 

type Radius = Float
type Position = (Float, Float)
type Degree = Float
type Speed = (Float, Float)

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

data GameState = Game {
	shipLoc :: Position,
	shipAng :: Float,
	shipVel :: (Float, Float),
	bullets :: [BulletState]
} deriving Show

data BulletState = Bullet {
	bulLoc :: Position,
	bullAng :: Float
} deriving Show