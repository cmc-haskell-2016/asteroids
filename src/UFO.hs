{-# LANGUAGE RecordWildCards #-}
module UFO
(
    UFO(..)
) where

import Types

ufoRad :: Radius
ufoRad = 10

data UFO = UFO {
    ufoLoc :: Position,
    ufoNewVel :: Speed,
    ufoVel :: Speed,
    ufoStage :: Int,
    ufoAlive :: Bool
} deriving (Show, Eq)

