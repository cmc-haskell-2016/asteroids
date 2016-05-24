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
import Data.Maybe

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
    | (level == 4) && ((mod step 100) == 99) = shootBoss bigBoss u
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
    if ((step == 2000) && (level < 4))
        then u {
            level = level + 1,
            step = 0
        }
        else u 

addBoss :: Universe -> Universe
addBoss u@Universe{..} 
    | (isNothing bigBoss) && (level == 4) = u {
        randLoc = drop 2 randLoc,
        randVel = drop 2 randVel,
        bigBoss = Just Boss {
            bossLoc = newLoc,
            bossVel = newVel,
            bossStage = 100,
            bossAlive = True
        }        
    }
    | otherwise = u 
    where
        newLoc = generateObjPosition ship randLoc
        randSpeed = take 2 randVel
        (xn, yn) = (head randSpeed, head (tail randSpeed))
        norm = sqrt (xn * xn + yn * yn)
        newVel = (xn / norm * bossSpeed, yn / norm * bossSpeed)


shootBoss :: Maybe Boss -> Universe -> Universe
shootBoss Nothing u = u
shootBoss (Just boss) u@Universe{..} = u {
    bullets = bullets ++ 
        zipWith (\ p v -> Bullet {bulLoc = p, bulVel = v, bulAlive = True, bulRad = 3}) poslist vellist
    }
    where
        (x, y) = bossLoc boss
        offset = bossRad + 30
        poslist = [
            (x + offset, y),
            (x + offset, y + offset),
            (x, y + offset),
            (x - offset, y + offset),
            (x - offset, y),
            (x - offset, y - offset),
            (x, y - offset),
            (x + offset, y - offset)] 
        vellist = map (\(xp, yp) -> ((xp - x) / offset * bulletSpeed, (yp - y) / offset * bulletSpeed)) poslist


addUFO :: Universe -> Universe
addUFO u@Universe{..} =
    if (((mod step 200) == 1) && (level /= 4)) 
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
        bigBoss = collBoss u bigBoss,
        bullets = map (checkCollisionsWithOthers u) bullets
    }

moveAllObjects :: Time -> Universe -> Universe
moveAllObjects sec u@Universe{..} =
    u {
        ship = move sec ship,
        asteroids = map (move sec) asteroids,
        bigBoss = moveBoss sec bigBoss,
--        ufos = (map (findShip ship) ufos),
        ufos = map (move sec) (map (findShip ship) ufos),
        bullets = map (move sec) bullets
    }

moveBoss :: Time -> Maybe Boss -> Maybe Boss
moveBoss _ Nothing = Nothing
moveBoss sec (Just boss) = Just (move sec boss) 

collBoss :: Universe -> Maybe Boss -> Maybe Boss
collBoss _ Nothing = Nothing
collBoss u@Universe{..} (Just boss) = Just (checkCollisionsWithOthers u boss) 

delBoss :: Maybe Boss -> Maybe Boss
delBoss Nothing = Nothing
delBoss (Just boss) 
    | (bossAlive boss) = Just boss
    | otherwise = Nothing



delObjects :: Universe -> Universe
delObjects u@Universe{..} =
    u {
        asteroids = (filter (\ast -> astAlive ast) asteroids),
        ufos = (filter (\ufo -> ufoAlive ufo) ufos),
        bullets = (filter (\bul -> bulAlive bul) bullets),
        bigBoss = delBoss bigBoss
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
            (changeLevel . addAsteroid . addUFO . addBoss . shooting . delObjects . updateObjects . checkCollisions . moveAllObjects sec)
