{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Game
(
    GameMode(..),
    GameState(..),
    updateGame,
    getGameState
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


data GameMode =
    Single GameState
    | Cooperative GameState

--add alternative states here, like 'pause', 'settings' and so on
data GameState =
    InGame [Universe]
    | Waiting Int
    | GameOver deriving (Generic, Read, Show)

instance ToJSON GameState
instance FromJSON GameState

instance WebSocketsData GameState where
    fromLazyByteString = read . BL8.unpack
    toLazyByteString   = BL8.pack . show


getGameState :: GameMode -> GameState
getGameState (Single gs) = gs
getGameState (Cooperative gs) = gs


generateAstPosition :: [Ship] -> [Float] -> Position
generateAstPosition ships (x:y:xs)
    | checkForAllShips ships (x, y) = generateAstPosition ships xs
    | otherwise = (x, y)
    where
        checkForAllShips :: [Ship] -> Position -> Bool
        checkForAllShips [] _ = False
        checkForAllShips (s:ss) pos =
            twoCirclesCollide (shipLoc s) (shipSize s) pos 50
            || checkForAllShips ss pos
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
        randLoc = generateAstPosition ships (randomRs ((-200)::Float, 200::Float) genX)
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
        ships = map (checkCollisionsWithOthers u) ships,
        asteroids = map (checkCollisionsWithOthers u) asteroids,
        bullets = map (checkCollisionsWithOthers u) bullets
    }


moveAllObjects :: Time -> Universe -> Universe
moveAllObjects sec u@Universe{..} =
    u {
        ships = map (move sec) ships,
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
        ships = map updateShip ships
    }


updateGame :: Time -> GameMode -> GameMode
updateGame sec (Single gs) = Single $ updateGameSingle sec gs
    where
        updateGameSingle :: Time -> GameState -> GameState
        updateGameSingle _ GameOver = GameOver
        updateGameSingle _ (InGame (u@Universe{..}:_))
            | not $ all shipAlive ships = GameOver
            | otherwise =
                InGame $ [chain $ u {
                    step = step + 1
                }]
        updateGameSingle _ _gs = _gs
        chain =
            (addAsteroid . delObjects . checkCollisions . updateObjects . moveAllObjects sec)
updateGame sec (Cooperative gs) = Cooperative $ updateGameCooperative sec gs
    where
        updateGameCooperative :: Time -> GameState -> GameState
        updateGameCooperative _ GameOver = GameOver
        updateGameCooperative _ (InGame (u@Universe{..}:_))
            | not $ all shipAlive ships = GameOver
            | otherwise =
                InGame $ [chain $ u {
                    step = step + 1
                }]
        updateGameCooperative _ _gs = _gs
        chain =
            (addAsteroid . delObjects . checkCollisions . updateObjects . moveAllObjects sec)
