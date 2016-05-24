{-# LANGUAGE RecordWildCards #-}
module GraphObject where

import Types
import Universe
import Collisions
import Ship
import Asteroid
import Bullet
import UFO

import Graphics.Gloss
import Data.Maybe


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
            color shipColor $
            rotate phi $
            polygon [(10, -5), (0, 0), (0, (shipSize s))],
            translate x y $
            color shipColor $
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

    shouldKill s u@Universe{..} =
        (((checkCollisionsWithMe sLoc 20 bullets) && (not (shieldOn ship)))
        || wallCollision sLoc 20
        || ((checkCollisionsWithMe sLoc 5 asteroids) && (not (shieldOn ship)))
        || ((checkCollisionsWithMe sLoc 5 ufos) && (not (shieldOn ship))))
        && not godMode
        where
            sLoc = shipLoc ship

    kill ship = ship {
        shipAlive = False
    }


instance GraphObject Bullet where
    draw bull =
            translate x y $
            color red $
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

    shouldKill bull u@Universe{..} =
        checkCollisionsWithMe (bulLoc bull) 3 asteroids
        || checkCollisionsWithMe (bulLoc bull) 3 (maybeToList bigBoss)
        || ((shieldOn ship) && (twoCirclesCollide (bulLoc bull) 3 sLoc sRad))
        where
            sLoc = shipLoc ship
            sRad = shieldRad ship

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

    shouldKill ast u@Universe{..} =
        (checkCollisionsWithMe aLoc aRad bullets)
 --       || (checkCollisionsWithMe aLoc aRad asteroids)
        || (checkCollisionsWithMe aLoc aRad ufos)
        || (wallCollision aLoc (aRad / 2))
        || ((shieldOn ship) && (twoCirclesCollide aLoc aRad sLoc sRad))
        where
            aLoc = astLoc ast
            aRad = astRad ast
            sLoc = shipLoc ship
            sRad = shieldRad ship

    kill ast = ast {
        astAlive = False
    }


instance GraphObject UFO where
    draw ufo =
        translate x y $
        color green $
        circleSolid ufoRad
        where
            (x, y) = ufoLoc ufo

    move sec ufo =
        ufo {
            ufoLoc = (x1, y1),
            ufoVel = ufoNewVel ufo
        }
        where
            (x, y) = ufoLoc ufo
            (vx, vy) = ufoNewVel ufo
            x1 = x + vx * sec
            y1 = y + vy * sec

    checkCollisionsWithOthers u obj
        | shouldKill obj u = kill obj
        | otherwise = obj

    checkCollisionsWithMe _ _ [] = False
    checkCollisionsWithMe pos rad (a:as)
        | (twoCirclesCollide pos rad (ufoLoc a) 10) = True
        | otherwise = checkCollisionsWithMe pos rad as

    shouldKill ufo u@Universe{..} =
        (checkCollisionsWithMe uLoc uRad bullets)
        || (checkCollisionsWithMe uLoc uRad asteroids)
        || (wallCollision uLoc (uRad / 2))
        || ((shieldOn ship) && (twoCirclesCollide uLoc uRad sLoc sRad))
        where
            uLoc = ufoLoc ufo
            uRad = ufoRad
            sLoc = shipLoc ship
            sRad = shieldRad ship

    kill ufo = ufo {
        ufoAlive = False
    }

colorBoss :: Int -> Color
colorBoss n
    | n > 90 = dark (dark (dark (dark red)))
    | n > 80 = dark (dark (dark red))
    | n > 70 = dark (dark red)
    | n > 60 = dark red
    | n > 50 = red
    | n > 40 = light red
    | n > 30 = light (light red)
    | n > 20 = light (light (light red))
    | n > 10 = light (light (light (light red)))
    |otherwise = white

instance GraphObject Boss where
    draw boss =
        translate x y $
        color (colorBoss (bossStage boss)) $
        circleSolid bossRad
        where
            (x, y) = bossLoc boss 
            

    move sec boss =
        boss {
            bossLoc = (x1, y1),
            bossVel = 
                if (wallCollision (x1, y1) bRad)
                    then (- vx ,- vy)
                    else bossVel boss
        }
        where
            (x, y) = bossLoc boss
            (vx, vy) = bossVel boss
            x1 = x + vx * sec
            y1 = y + vy * sec
            bRad = bossRad

    checkCollisionsWithOthers u@Universe{..} obj 
        | ((bossStage obj) <= 0)  = kill obj
        |(checkCollisionsWithMe bLoc bRad bullets)
            || ((shieldOn ship) && (twoCirclesCollide bLoc bRad sLoc sRad))
            = obj {bossStage = (bossStage obj) -1}
        | otherwise = obj
        where
            bLoc = bossLoc obj
            bRad = bossRad
            sLoc = shipLoc ship
            sRad = shieldRad ship

    checkCollisionsWithMe _ _ [] = False
    checkCollisionsWithMe pos rad (a:as)
        | (twoCirclesCollide pos rad (bossLoc a) bossRad) = True
        | otherwise = checkCollisionsWithMe pos rad as

    shouldKill boss _ = 
        ((bossStage boss) <= 0)

    kill boss = boss {
        bossAlive = False
    }