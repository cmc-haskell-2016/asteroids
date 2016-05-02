module Game where

import Types

import System.Random


width :: Int
width = 700

height :: Int
height = 500

maxShieldPower:: Int
maxShieldPower = 70

bulletSpeed :: Float
bulletSpeed = 200


generateAstPosition :: Ship -> [Float] -> Position
generateAstPosition ship (x:xs) =
    if (twoCirclesCollide (shipLoc ship) (shipSize ship) (x, head xs) 50)
        then generateAstPosition ship (tail xs)
        else (x, head xs)


addAsteroid :: GameState -> GameState
addAsteroid (Game 60 s ast b) = Game 0 s (a : ast) b
    where
        genX = mkStdGen (round (foldr (+) 1 (map (fst . astLoc) ast)))
        randLoc = generateAstPosition s (randomRs ((-200)::Float, 200::Float) genX)
        genY = mkStdGen (round (foldr (+) 1 (map (snd . astLoc) ast)))
        randSpeed = take 2 (randomRs ((-70)::Float, 70::Float) genY)
        randVel = (head randSpeed, head (tail randSpeed))
        genAst = mkStdGen (length ast)
        randInt = take 1 (randomRs (10::Float, 50::Float) genAst)
        randRad = head randInt
        a = Asteroid {astLoc = randLoc, astAng = 0, astSize = randRad, astAlive = True, astVel = randVel}
addAsteroid game = game


moveShip :: Float -> GameState -> GameState
moveShip sec (Game t s a b) =
    if ((not (shipAlive s)) ||
        (wallCollision (x, y) 20) ||
        (asteroidCollision (x, y) 20 a))
        then
            deathState
        else
            if shieldOn s then
                if shieldAcc s == 0
                    then
                        moveShipAccel sec $ Game t (s {shipAng = newAng, shipLoc = (x1, y1), shieldOn = False}) a b
                    else
                        moveShipAccel sec  $ Game t (s {shipAng = newAng, shipLoc = (x1, y1), shieldAcc = shieldAcc s - 1}) a b
            else
                if shieldAcc s >= maxShieldPower
                    then
                        moveShipAccel sec $ Game t (s {shipAng = newAng, shipLoc = (x1, y1)}) a b
                    else
                        moveShipAccel sec $ Game t (s {shipAng = newAng, shipLoc = (x1, y1), shieldAcc = shieldAcc s + 1}) a b
        where
            (x, y) = shipLoc s
            v = shipVel s
            newAng = (shipAng s) + ((rotation s) / 1.5)
            x1 = x + v* (sin (newAng*pi/180)) * sec
            y1 = y + v* (cos (newAng*pi/180)) * sec


moveShipAccel :: Float -> GameState -> GameState
moveShipAccel sec (Game t s a b) =
    if  shipAccel s
        then Game t (s {shipAng = newAng, shipLoc = (x1, y1), shipVel = shipVel s  + 3 }) a b
        else Game t (s {shipAng = newAng, shipLoc = (x1, y1), shipVel = shipVel s - (shipVel s)*0.02}) a b
    where
        (x, y) = shipLoc s
        v = shipVel s
        newAng = (shipAng s) + ((rotation s) / 1.5)
        x1 = x + v* (sin (newAng*pi/180)) * sec
        y1 = y + v* (cos (newAng*pi/180)) * sec


moveAllBullets :: Time -> GameState -> GameState
moveAllBullets sec (Game t s a b) =
    Game t s a (map (\bull ->
        if asteroidCollision (bulLoc bull) 3 a
            then bull {bulAlive = False}
            else moveBullet sec bull) b)


moveBullet :: Time -> Bullet -> Bullet
moveBullet sec bull =
    if (wallCollision (x,y) 3)
        then bull {bulAlive = False}
        else bull {bulLoc = (x1, y1)}
    where
    (x, y) = bulLoc bull
    (vx, vy) = bulVel bull
    x1 = x + vx * sec
    y1 = y + vy * sec


moveAllAsteroids :: Time -> GameState -> GameState
moveAllAsteroids sec (Game t s a b) =
    Game t s (map (\ast ->
        if (bulletsCollision (astLoc ast) (astSize ast) b) ||
            (shieldOn s) &&
            (twoCirclesCollide (astLoc ast) (astSize ast) (shipLoc s) (shieldRad s))
            then ast {astAlive = False}
            else moveAsteroid sec ast) a) b


moveAsteroid :: Time -> Asteroid -> Asteroid
moveAsteroid sec ast =
    if (wallCollision (x,y) ((astSize ast) / 2))
        then ast {astAlive = False}
        else ast {astLoc = (x1, y1)}
    where
        (x, y) = astLoc ast
        (vx, vy) = astVel ast
        x1 = x + vx * sec
        y1 = y + vy * sec


moveObjects :: Time -> GameState -> GameState
moveObjects sec game = ((moveShip sec) . (moveAllBullets sec) . (moveAllAsteroids sec)) game


delObjects :: GameState -> GameState
delObjects (Game t s a b) = Game t s (filter (\ast -> astAlive ast) a) (filter (\bul -> bulAlive bul) b)


twoCirclesCollide :: Position -> Radius -> Position -> Radius -> Bool
twoCirclesCollide (x1, y1) rad1 (x2, y2) rad2 =
    if dist > rad1 + rad2
        then False
        else True
    where
        dist = sqrt ((x1-x2)^2 + (y1-y2)^2)


asteroidCollision :: Position -> Radius -> [Asteroid] -> Bool
asteroidCollision _ _ [] = False
asteroidCollision shipPos rad (a:as) =
    if (twoCirclesCollide shipPos rad (astLoc a) (astSize a))
        then True
        else asteroidCollision shipPos rad as


bulletsCollision :: Position -> Radius -> [Bullet] -> Bool
bulletsCollision _ _ [] = False
bulletsCollision shipPos rad (a:as) =
    if (twoCirclesCollide shipPos rad (bulLoc a) (bulSize a))
        then True
        else bulletsCollision shipPos rad as


wallCollision :: Position -> Radius -> Bool
wallCollision pos rad = xCollision pos rad || yCollision pos rad


xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad =
    (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)


yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad =
    (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)


initShip :: Ship
initShip = Ship {
    shipSize = 20,
    shipLoc = (0, 0),
    shipAng = 0,
    rotation = 0,
    shipVel = 0,
    shipAlive = True,
    shipAccel = False,
    shieldOn = False,
    shieldAcc = 0,
    shieldRad = 40
}


deathShip :: Ship
deathShip = Ship {
    shipSize = 20,
    shipLoc = (0, 0),
    shipAng = 0,
    rotation = 0,
    shipVel = 0,
    shipAlive = False,
    shipAccel = False,
    shieldOn = False,
    shieldAcc = 0,
    shieldRad = 40
}


initBullet :: Ship -> Bullet
initBullet s = Bullet {
    bulLoc = shipLoc s,
    bulSize = 3,
    bulAng = shipAng s,
    bulAlive = True,
    bulVel = velang
}
    where
        yvel = cos ((shipAng s) * pi / 180)
        xvel = sin ((shipAng s) * pi / 180)
        norm = sqrt (xvel * xvel + yvel * yvel)
        velang = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)


initialState :: GameState
initialState = Game 0 initShip [] []


deathState :: GameState
deathState = Game 0 deathShip [] []


updateGame :: Time -> GameState -> GameState
updateGame sec (Game t s a b) = (addAsteroid . moveObjects sec . delObjects) (Game (t + 1) s a b)
