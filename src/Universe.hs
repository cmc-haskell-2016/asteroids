module Universe where

import Ship
import Bullet
import Asteroid
import Types


data Universe = Universe {
    step :: Step,
    ship :: Ship,
    asteroids :: [Asteroid],
    bullets :: [Bullet]
} deriving (Show, Read, Generic)

initUniverse :: Universe
initUniverse = Universe {
    step = 0,
    ship = initShip,
    asteroids = [],
    bullets = []
}
