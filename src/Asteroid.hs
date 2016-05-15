{-# LANGUAGE DeriveGeneric #-}

module Asteroid
(
    Asteroid(..)
) where

import Types

data Asteroid = Asteroid {
    astLoc :: Position,
    astAng :: Degree,
    astRad :: Radius,
    astVel :: Speed,
    astAlive :: Bool
} deriving (Show, Eq, Generic)

instance ToJSON Asteroid
instance FromJSON Asteroid

