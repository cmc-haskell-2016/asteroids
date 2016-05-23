{-# LANGUAGE RecordWildCards #-}
module UFO
(
    UFO(..),
    ufoRad,
    ufoSpeed
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
    ufoStage :: Int,
    ufoAlive :: Bool
} deriving (Show, Eq)



