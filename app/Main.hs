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
renderPic (Game _ s a b) = pictures ([(drawingShip s)] ++ (map drawingAsteroid a) ++ (map drawingBullet b))

addAsteroid :: GameState -> GameState
addAsteroid (Game 180 s ast b) = Game 180 s (a : ast) b
    where
        genShipX = mkStdGen (round (fst (shipLoc s) + foldr (+) 1 (map (fst . astLoc) ast)))
        randPos = take 2 (randomRs ((-200)::Float, 200::Float) genShipX)
        randLoc = (head randPos, head (tail randPos))
        genShipY = mkStdGen (round (snd (shipLoc s)))
        randSpeed = take 2 (randomRs ((-70)::Float, 70::Float) genShipY)
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
updateGame sec (Game t s a b) = addAsteroid (moveObjects sec (delObjects (Game (updateStep t) s a b)))

updateStep :: Step -> Step
updateStep 181 = 0
updateStep t = t + 1

moveObjects :: Time -> GameState -> GameState
moveObjects sec game = moveShip sec (moveBullets sec (moveAsteroids sec game))

delObjects :: GameState -> GameState
delObjects (Game t s a b) = Game t s (filter (\ast -> astAlive ast) a) (filter (\bul -> bulAlive bul) b)

asteroidCollision :: Position -> Radius -> [Asteroid] -> Bool
asteroidCollision _ _ [] = False
asteroidCollision (x, y) rad (a:as) =
    if dist > rad + astSize a
        then asteroidCollision (x, y) rad as
        else True
    where
        dist = sqrt (diffX*diffX + diffY*diffY)
        astX = fst (astLoc a)
        astY = snd (astLoc a)
        diffX = x - astX
        diffY = y - astY

bulletsCollision :: Position -> Radius -> [Bullet] -> Bool
bulletsCollision _ _ [] = False
bulletsCollision (x, y) rad (a:as) =
    if dist > rad + 3
        then bulletsCollision (x, y) rad as
        else True
    where
        dist = sqrt ((x - (fst (bulLoc a)))^2 + (y - (snd (bulLoc a)))^2)

wallCollision :: Position -> Radius -> Bool
wallCollision pos rad = xCollision pos rad || yCollision pos rad

xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad = (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)

yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad = (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)


handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game t s a b) = Game t (s {shipVel = (shipVel s) + 70}) a b
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (Game t s a b) = Game t (s {shipVel = (shipVel s) - 70}) a b
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
