module Universe where

import Ship
import Bullet
import Asteroid
import UFO
import Types

import System.Random


data Universe = Universe {
    step :: Step,
    ship :: Ship,
    asteroids :: [Asteroid],
    ufos :: [UFO],
    bullets :: [Bullet],
    randLoc :: [Float],
    randVel :: [Float],
    randRad :: [Float],
    level :: Int,
    bigBoss :: Maybe Boss,
    godMode :: Bool
}

initUniverseIO :: IO Universe
initUniverseIO = do
    gen <- getStdGen
    randLoc <- return (randomRs ((-200)::Float, 200::Float) gen)
    randVel <- return (randomRs ((-70)::Float, 70::Float) gen)
    randRad <- return (randomRs (10::Float, 50::Float) gen) 
    return Universe {
        step = 0,
        ship = initShip,
        asteroids = [],
        ufos = [],
        bullets = [],
        randLoc = randLoc,
        randVel = randVel,
        randRad = randRad,
        level = 1,
        bigBoss = Nothing,
        godMode = False
    }

