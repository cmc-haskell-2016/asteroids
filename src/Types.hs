module Types where

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

--speedShip :: Float
--speedShip = 100

data GameState =
    Game {
        step :: Step,
        ship :: Ship,
        asteroids :: [Asteroid],
        bullets :: [Bullet]
    }
    | GameOver
    | Settings
    | Pause


data Ship = Ship {
    shipSize :: Float,
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
    astAlive :: Bool,
    astVel :: Speed
} deriving (Show, Eq)

data Bullet = Bullet {
    bulLoc :: Position,
    bulSize :: Float,
    bulAng :: Degree,
    bulVel :: Speed,
    bulAlive :: Bool
} deriving (Show, Eq)

