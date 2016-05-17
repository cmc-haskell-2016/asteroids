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
} deriving (Show, Eq, Read, Generic)

instance ToJSON Asteroid
instance FromJSON Asteroid

instance WebSocketsData Asteroid where
    fromLazyByteString = read . BL8.unpack
    toLazyByteString   = BL8.pack . show
