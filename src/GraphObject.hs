{-# LANGUAGE RecordWildCards #-}
module GraphObject where

import Types
import Universe
import Collisions
import Ship
import Asteroid
import Bullet

import Graphics.Gloss


class GraphObject a where
    draw :: a -> Picture
    move :: Time -> a -> a
    --check collisions for GraphObject a with other objects
    checkCollisionsWithOthers :: Universe -> a -> a
    --check collisions for an object with all GraphObject a's
    checkCollisionsWithMe :: Position -> Radius -> [a] -> Bool
    shouldKill :: a -> Universe -> Bool
    kill :: a -> a


instance GraphObject Ship where
    draw s =
        pictures[
            translate x y $
            color (shipColors (shipColor s)) $
            rotate phi $
            polygon [(10, -5), (0, 0), (0, (shipSize s))],
            translate x y $
            color (shipColors (shipColor s)) $
            rotate phi $
            polygon [(-10, -5), (0, 0), (0, (shipSize s))]]
        where
            (x, y) = shipLoc s
            phi = shipAng s

    move sec s =
        accelerate sec s {
            shipAng = newAng,
            shipLoc = (x1, y1)
        }
        where
            (x, y) = shipLoc s
            v = shipVel s
            newAng = (shipAng s) + ((rotation s) / 1.5)
            x1 = x + v* (sin (newAng*pi/180)) * sec
            y1 = y + v* (cos (newAng*pi/180)) * sec

    checkCollisionsWithOthers u obj
        | shouldKill obj u = kill obj
        | otherwise = obj

    checkCollisionsWithMe _ _ [] = False
    checkCollisionsWithMe pos rad (s:_) = twoCirclesCollide pos rad sLoc sRad
        where
            sLoc = shipLoc s
            sRad = shipSize s

    shouldKill s _u@Universe{..} =
        wallCollision sLoc 20
        || ((checkCollisionsWithMe sLoc 20 asteroids) && (not (shieldOn s)))
        where
            sLoc = shipLoc s

    kill ship = ship {
        shipAlive = False
    }


instance GraphObject Bullet where
    draw bull =
            translate x y $
            color (shipColors (bulColor bull)) $
            circleSolid 3
        where
            (x, y) = bulLoc bull

    move sec bull =
        bull {
            bulLoc = (x1, y1)
        }
        where
            (x, y) = bulLoc bull
            (vx, vy) = bulVel bull
            x1 = x + vx * sec
            y1 = y + vy * sec

    checkCollisionsWithOthers u obj
        | shouldKill obj u = kill obj
        | otherwise = obj

    checkCollisionsWithMe _ _ [] = False
    checkCollisionsWithMe pos rad (b:bs)
        | (twoCirclesCollide pos rad bLoc bRad) = True
        | otherwise = checkCollisionsWithMe pos rad bs
        where
            bLoc = bulLoc b
            bRad = bulRad b

    shouldKill bull _u@Universe{..} =
        wallCollision bLoc bRad
        || checkCollisionsWithMe bLoc bRad asteroids
        where
            bLoc = bulLoc bull
            bRad = bulRad bull

    kill bull = bull {
        bulAlive = False
    }


instance GraphObject Asteroid where
    draw ast =
        translate x y $
        color black $
        circleSolid rad
        where
            (x, y) = astLoc ast
            rad = astRad ast

    move sec ast =
        ast {
            astLoc = (x1, y1)
        }
        where
            (x, y) = astLoc ast
            (vx, vy) = astVel ast
            x1 = x + vx * sec
            y1 = y + vy * sec

    checkCollisionsWithOthers u obj
        | shouldKill obj u = kill obj
        | otherwise = obj

    checkCollisionsWithMe _ _ [] = False
    checkCollisionsWithMe pos rad (a:as)
        | (twoCirclesCollide pos rad (astLoc a) (astRad a)) = True
        | otherwise = checkCollisionsWithMe pos rad as

    shouldKill ast _u@Universe{..} =
        (checkCollisionsWithMe (astLoc ast) (astRad ast) bullets)
        || (wallCollision (astLoc ast) ((astRad ast) / 2))
        || collidesWithShip ships (astLoc ast) (astRad ast)
        where
            collidesWithShip :: [Ship] -> Position -> Radius -> Bool
            collidesWithShip [] _ _ = False
            collidesWithShip (s:xs) aLoc aRad
                | ((shieldOn s) && (twoCirclesCollide aLoc aRad sLoc sRad)) = True
                | otherwise = collidesWithShip xs aLoc aRad
                where
                    sLoc = shipLoc s
                    sRad = shieldRad s

    kill ast = ast {
        astAlive = False
    }