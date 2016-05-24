{-# LANGUAGE RecordWildCards #-}
module UFO
(
    UFO(..),
    Boss(..),
    ufoRad,
    bossRad,
    ufoSpeed,
    bossSpeed
) where

import Types

ufoRad :: Radius
ufoRad = 10

ufoSpeed :: Float
ufoSpeed = 40

data UFO = UFO {
    ufoLoc :: Position,
    ufoNewVel :: Speed,
    ufoVel :: Speed,
    ufoAlive :: Bool
} deriving (Show, Eq)

bossRad :: Radius
bossRad = 80

bossSpeed :: Float
bossSpeed = 30

data Boss = Boss {
    bossLoc :: Position,
--    ufoNewVel :: Speed,
    bossVel :: Speed,
    bossStage :: Int,
    bossAlive :: Bool
} deriving (Show, Eq)

