{-# LANGUAGE RecordWildCards #-}
module Game
(
    GameState(..),
    Universe(..),
    updateGame,
    width,
    height,
    initUniverseIO,
    findShip
) where

import Types
import GraphObject
import Ship
import Asteroid
import UFO
import Bullet
import Universe
import Collisions

import System.Random

--add alternative states here, like 'pause', 'settings' and so on
data GameState =
    InGame Universe
    | GameOver



generateObjPosition :: Ship -> [Float] -> Position
generateObjPosition ship (x:y:xs)
    | twoCirclesCollide (shipLoc ship) (shipSize ship) (x, y) 150 =
        generateObjPosition ship xs
    | otherwise = (x, y)
-- TODO: should be handling this case more appropriately
generateObjPosition _ [] = (0, 0)
generateObjPosition _ [_] = (0, 0)

shotUFO :: [UFO] -> Position -> [Bullet]
shotUFO [] _ = []
shotUFO (x:xs) ship = b : (shotUFO xs ship)
    where
        xvel = (fst ship) - (fst (ufoLoc x))
        yvel = (snd ship) - (snd (ufoLoc x))
        norm = sqrt (xvel * xvel + yvel * yvel)
        vel = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)
        loc = ((fst (ufoLoc x)) + xvel / norm * 10, (snd (ufoLoc x)) + yvel / norm * 10)
        b = Bullet {
            bulLoc = loc,
            bulRad = 3,
            bulAlive = True,
            bulVel = vel
        }
shooting :: Universe -> Universe
shooting u@Universe{..}
    | (level == 2) && ((mod step 100) == 40) = u {
        bullets = bullets ++ (shotUFO ufos (shipLoc ship))}
    | (level == 3) && ((mod step 50) == 40) = u {
        bullets = bullets ++ (shotUFO ufos (shipLoc ship))}
    | otherwise = u

findShip :: Ship -> UFO -> UFO
findShip s u = u {
    ufoNewVel = vel
    }
    where
        (xs, ys) = shipLoc s
        (xu, yu) = ufoLoc u
        xvel = xs - xu
        yvel = ys - yu
        norm = sqrt (xvel * xvel + yvel * yvel)
        vel = (xvel /norm * ufoSpeed, yvel /norm * ufoSpeed)


changeLevel :: Universe -> Universe
changeLevel u@Universe{..} =
    if ((step == 2000) && (level < 3))
        then u {
            level = level + 1,
            step = 0
        }
        else u 

addUFO :: Universe -> Universe
addUFO u@Universe{..} =
    if ((mod step 200) == 1) 
        then u {
 --           step = 0,
            ufos = (a : ufos),
            randLoc = drop 4 randLoc,
            randVel = drop 2 randVel
        }
        else u
    where
        newLoc = generateObjPosition ship randLoc
        randSpeed = take 2 randVel
        newVel = (head randSpeed, head (tail randSpeed))
        a = UFO {ufoLoc = newLoc, ufoAlive = True, ufoVel = newVel, ufoNewVel = newVel}

addAsteroid :: Universe -> Universe
addAsteroid u@Universe{..} =
    if ((mod step 50) == 1) 
        then u {
            asteroids = (a : asteroids),
            randLoc = drop 2 randLoc,
            randVel = drop 2 randVel,
            randRad = drop 1 randRad
        }
        else u
    where
        newLoc = generateObjPosition ship randLoc
        randSpeed = take 2 randVel
        newVel = (head randSpeed, head (tail randSpeed))
        newRad = head (take 1 randRad)
        a = Asteroid {astLoc = newLoc, astRad = newRad, astAlive = True, astVel = newVel}


--check collisions for all objects
checkCollisions :: Universe -> Universe
checkCollisions u@Universe{..} =
    u {
        ship = checkCollisionsWithOthers u ship,
        asteroids = map (checkCollisionsWithOthers u) asteroids,
        ufos = map (checkCollisionsWithOthers u) ufos,
        bullets = map (checkCollisionsWithOthers u) bullets
    }

moveAllObjects :: Time -> Universe -> Universe
moveAllObjects sec u@Universe{..} =
    u {
        ship = move sec ship,
        asteroids = map (move sec) asteroids,
--        ufos = (map (findShip ship) ufos),
        ufos = map (move sec) (map (findShip ship) ufos),
        bullets = map (move sec) bullets
    }



delObjects :: Universe -> Universe
delObjects u@Universe{..} =
    u {
        asteroids = (filter (\ast -> astAlive ast) asteroids),
        ufos = (filter (\ufo -> ufoAlive ufo) ufos),
        bullets = (filter (\bul -> bulAlive bul) bullets)
    }


updateObjects :: Universe -> Universe
updateObjects u@Universe{..} =
    u {
        ship = updateShip ship
    }


updateGame :: Time -> GameState -> GameState
updateGame _ GameOver = GameOver
updateGame sec (InGame u@Universe{..})
    | not $ shipAlive ship = GameOver
    | otherwise =
        InGame $ chain $ u {
            step = step + 1
        }
    where
        chain =
            (changeLevel . addAsteroid . addUFO . shooting . delObjects . checkCollisions . updateObjects . moveAllObjects sec)
