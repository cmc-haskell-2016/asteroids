{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Game
(
    GameState(..),
    Universe(..),
    updateGame,
    width,
    height,
    initUniverse
) where

import Types
import GraphObject
import Ship
import Asteroid
import Bullet
import Universe
import Collisions

import System.Random
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.WebSockets
import System.Exit

--add alternative states here, like 'pause', 'settings' and so on
data GameState =
    InGame Universe
    | GameOver

instance ToJSON GameState
instance FromJSON GameState

instance WebSocketsData GameState where
    fromLazyByteString = read . BL8.unpack
    toLazyByteString   = BL8.pack . show

generateAstPosition :: Ship -> [Float] -> Position
generateAstPosition ship (x:y:xs)
    | twoCirclesCollide (shipLoc ship) (shipSize ship) (x, y) 50 =
        generateAstPosition ship xs
    | otherwise = (x, y)
-- TODO: should be handling this case more appropriately
generateAstPosition _ [] = (0, 0)
generateAstPosition _ [_] = (0, 0)


addAsteroid :: Universe -> Universe
addAsteroid u@Universe{..} =
    if (step == 60)
        then u {
            step = 0,
            asteroids = (a : asteroids)
        }
        else u
    where
        genX = mkStdGen (round (foldr (+) 1 (map (fst . astLoc) asteroids)))
        randLoc = generateAstPosition ship (randomRs ((-200)::Float, 200::Float) genX)
        genY = mkStdGen (round (foldr (+) 1 (map (snd . astLoc) asteroids)))
        randSpeed = take 2 (randomRs ((-70)::Float, 70::Float) genY)
        randVel = (head randSpeed, head (tail randSpeed))
        genAst = mkStdGen (length asteroids)
        randInt = take 1 (randomRs (10::Float, 50::Float) genAst)
        randRad = head randInt
        a = Asteroid {astLoc = randLoc, astAng = 0, astRad = randRad, astAlive = True, astVel = randVel}


--check collisions for all objects
checkCollisions :: Universe -> Universe
checkCollisions u@Universe{..} =
    u {
        ship = checkCollisionsWithOthers u ship,
        asteroids = map (checkCollisionsWithOthers u) asteroids,
        bullets = map (checkCollisionsWithOthers u) bullets
    }


--check collisions for an object with the wall
wallCollision :: Position -> Radius -> Bool
wallCollision pos rad = xWallCollision pos rad || yWallCollision pos rad


xWallCollision :: Position -> Radius -> Bool
xWallCollision (x, _) rad =
    (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)


yWallCollision :: Position -> Radius -> Bool
yWallCollision (_, y) rad =
    (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)


twoCirclesCollide :: Position -> Radius -> Position -> Radius -> Bool
twoCirclesCollide (x1, y1) rad1 (x2, y2) rad2 =
    if dist > rad1 + rad2
        then False
        else True
    where
        dist_x = (x1-x2)^2
        dist_y = (y1-y2)^2
        dist = sqrt (dist_x + dist_y)


moveAllObjects :: Time -> GameState -> GameState
moveAllObjects sec game@Game{..} =
    game {
        ship = move sec ship,
        asteroids = map (move sec) asteroids,
        bullets = map (move sec) bullets
    }


delObjects :: Universe -> Universe
delObjects u@Universe{..} =
    u {
        asteroids = (filter (\ast -> astAlive ast) asteroids),
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
            (addAsteroid . delObjects . checkCollisions . updateObjects . moveAllObjects sec)
