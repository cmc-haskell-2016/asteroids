module Collisions where

import Types

width :: Int
width = 700

height :: Int
height = 500


--check collisions for an object with the wall
wallCollision :: Position -> Radius -> Bool
wallCollision pos rad = xWallCollision || yWallCollision
    where
        (x, y) = pos
        xWallCollision =
            (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)
        yWallCollision =
            (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)


twoCirclesCollide :: Position -> Radius -> Position -> Radius -> Bool
twoCirclesCollide (x1, y1) rad1 (x2, y2) rad2 = dist <= rad1 + rad2
    where
        dist = sqrt ((x1-x2)^2 + (y1-y2)^2)
