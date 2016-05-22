module Universe where

import Ship
import Bullet
import Asteroid
import Types


data Universe = Universe {
    step :: Step,
    ship :: Ship,
    asteroids :: [Asteroid],
    bullets :: [Bullet],
    randLoc :: [Float],
    randVel :: [Float],
    randRad :: [Float]
}

initUniverse :: [Float] -> [Float] -> [Float] -> Universe
initUniverse loc vel rad = Universe {
    step = 0,
    ship = initShip,
    asteroids = [],
    bullets = [],
    randLoc = loc,
    randVel = vel,
    randRad = rad
}
