module Main where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Rendering
import System.Random

window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)


drawingShip :: Ship -> Picture
drawingShip ship = if shipAlive ship
    then
        pictures[
        translate x y $
        color shipColor $
        rotate phi $
        polygon [(10, -5), (0, 0), (0, 20)],
        translate x y $
        color shipColor $
        rotate phi $
        polygon [(-10, -5), (0, 0), (0, 20)]]
    else
        scale 10 10 (pictures[
        translate x y $
        color shipColor $
        rotate phi $
        polygon [(10, -5), (0, 0), (0, 20)],
        translate x y $
        color shipColor $
        rotate phi $
        polygon [(-10, -5), (0, 0), (0, 20)],
        translate (-18) (-20) $
        scale 0.05 0.05 $
        color red $
        text "Game Over"])
    where
        (x, y) = shipLoc ship
        phi = shipAng ship


drawingAsteroid :: Asteroid -> Picture
drawingAsteroid ast =
        translate x y $
        color black $
        circleSolid rad
    where
        (x, y) = astLoc ast
        rad = astSize ast

drawingBullet :: Bullet -> Picture
drawingBullet bull =
        translate x y $
        color red $
        circleSolid 3
    where
        (x, y) = bulLoc bull

initialShip :: Ship
initialShip = Ship {
    shipLoc = (0, 0),
    shipAng = 0,
    rotation = 0,
    shipVel = 0,
    shipAlive = True
}

deathShip :: Ship
deathShip = Ship {
    shipLoc = (0, 0),
    shipAng = 0,
    rotation = 0,
    shipVel = 0,
    shipAlive = False
}

initialState :: GameState
initialState = Game 0 initialShip [] []

deathState :: GameState
deathState = Game 0 deathShip [] []

renderPic :: GameState -> Picture
renderPic (Game _ s a b) = pictures ((drawingShip s) : (map drawingAsteroid a) ++ (map drawingBullet b))

{-
generateAstPosition :: Position -> [Float] -> Position
generateAstPosition (xSh, ySh) x:xs =
    if (x == xSh) && (take 1 xs == ySh)
-}

addAsteroid :: GameState -> GameState
addAsteroid (Game 60 s ast b) = Game 0 s (a : ast) b
    where
        genX = mkStdGen (round (foldr (+) 1 (map (fst . astLoc) ast)))
        randPos = take 2 (randomRs ((-200)::Float, 200::Float) genX)
        randLoc = (head randPos, head (tail randPos))
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
    if ((not (shipAlive s)) || (wallCollision (x, y) 20) || (asteroidCollision (x, y) 20 a))
        then deathState
        else Game t (s {shipAng = newAng, shipLoc = (x1, y1)}) a b
    where
        (x, y) = shipLoc s
        v = shipVel s
        newAng = (shipAng s) + ((rotation s) / 1.5)
        x1 = x + v* (sin (newAng*pi/180)) * sec
        y1 = y + v* (cos (newAng*pi/180)) * sec


moveBullets :: Time -> GameState -> GameState
moveBullets sec (Game t s a b) =
    Game t s a (map (\bull ->
        if asteroidCollision (bulLoc bull) 3 a
            then    bull {bulAlive = False}
            else    moveBull sec bull) b)


moveBull :: Time -> Bullet -> Bullet
moveBull sec bull =
    if (wallCollision (x,y) 3)
        then bull {bulAlive = False}
        else bull {bulLoc = (x1, y1)}
    where
    (x, y) = bulLoc bull
    (vx, vy) = bulVel bull
    x1 = x + vx * sec
    y1 = y + vy * sec

moveAsteroids :: Time -> GameState -> GameState
moveAsteroids sec (Game t s a b) =
    Game t s (map (\ast ->
        if bulletsCollision (astLoc ast) (astSize ast) b
            then    ast {astAlive = False}
            else    moveAst sec ast) a) b

moveAst :: Time -> Asteroid -> Asteroid
moveAst sec ast =
    if (wallCollision (x,y) ((astSize ast) / 2))
        then ast {astAlive = False}
        else ast {astLoc = (x1, y1)}
    where
        (x, y) = astLoc ast
        (vx, vy) = astVel ast
        x1 = x + vx * sec
        y1 = y + vy * sec


updateGame :: Time -> GameState -> GameState
updateGame sec (Game t s a b) = addAsteroid $ moveObjects sec $ delObjects (Game (t + 1) s a b)


moveObjects :: Time -> GameState -> GameState
moveObjects sec game = moveShip sec (moveBullets sec (moveAsteroids sec game))

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
    if (twoCirclesCollide shipPos rad (astLoc a) (astSize a)) == True
        then True
        else asteroidCollision shipPos rad as

bulletsCollision :: Position -> Radius -> [Bullet] -> Bool
bulletsCollision _ _ [] = False
bulletsCollision shipPos rad (a:as) =
    if (twoCirclesCollide shipPos rad (bulLoc a) bulletSize) == True
        then True
        else bulletsCollision shipPos rad as


wallCollision :: Position -> Radius -> Bool
wallCollision pos rad = xCollision pos rad || yCollision pos rad

xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad = (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)

yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad = (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)


handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game t s a b) = Game t (s {shipVel = (shipVel s) + speedShip}) a b
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (Game t s a b) = Game t (s {shipVel = (shipVel s) - speedShip}) a b
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (Game t s a b) = Game t (s {rotation = (rotation s) - 5}) a b
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (Game t s a b) = Game t (s {rotation = (rotation s) + 5}) a b
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) (Game t s a b) = Game t (s {rotation = (rotation s) + 5}) a b
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) (Game t s a b) = Game t (s {rotation = (rotation s) - 5}) a b
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (Game t s a b) = Game t s a ((Bullet {bulLoc = (x, y), bulAng = ang, bulAlive = True, bulVel = velang}) : b)
    where
        (x, y) = shipLoc s
        ang = shipAng s
        yvel = cos ((shipAng s) * pi / 180)
        xvel = sin ((shipAng s) * pi / 180)
        norm = sqrt (xvel * xvel + yvel * yvel)
        velang = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)
handleKeys _ game = game



main :: IO ()
main = play window background fps initialState renderPic handleKeys updateGame
