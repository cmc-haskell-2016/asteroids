{-# LANGUAGE RecordWildCards #-}
module Game
(
    GameState(..),
    updateGame,
    width,
    height,
    initGame
) where

import Types
import GraphObject
import Ship
import Asteroid
import Bullet

import System.Random

--add alternative states here, like 'pause', 'settings' and so on
data GameState =
    Game {
        step :: Step,
        ship :: Ship,
        asteroids :: [Asteroid],
        bullets :: [Bullet]
    }
    | GameOver


width :: Int
width = 700

height :: Int
height = 500


initGame :: GameState
initGame = Game {
    step = 0,
    ship = initShip,
    asteroids = [],
    bullets = []
}


checkGameOver :: GameState -> GameState
checkGameOver game@Game{..} =
    if (shipAlive ship)
        then game
        else GameOver
checkGameOver GameOver = GameOver


generateAstPosition :: Ship -> [Float] -> Position
generateAstPosition ship (x:xs) =
    if (twoCirclesCollide (shipLoc ship) (shipSize ship) (x, head xs) 50)
        then generateAstPosition ship (tail xs)
        else (x, head xs)


addAsteroid :: GameState -> GameState
addAsteroid game@Game{..} =
    if (step == 60)
    then
        game {
            step = 0,
            asteroids = (a : asteroids)
        }
    else
        game
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
addAsteroid GameOver = GameOver


--check collisions for all objects
checkCollisions :: GameState -> GameState
checkCollisions game@Game{..} =
    (checkShipCollision . checkBulletsCollisions . checkAsteroidsCollisions) game
checkCollisions GameOver = GameOver


--check collisions for asteroids with other objects
checkAsteroidsCollisions :: GameState -> GameState
checkAsteroidsCollisions game@Game{..} =
    game {
        asteroids =
            (map
                (\ast ->
                    if (collisionWithBullets (astLoc ast) (astRad ast) bullets)
                        || (wallCollision (astLoc ast) ((astRad ast) / 2))
                        || ((shieldOn ship)
                        && (twoCirclesCollide (astLoc ast) (astRad ast) sLoc sRad))
                        then ast { astAlive = False }
                        else ast
                )
                asteroids
            )
    }
    where
        sLoc = shipLoc ship
        sRad = shieldRad ship
checkAsteroidsCollisions GameOver = GameOver


--check collisions for bullets with other objects
checkBulletsCollisions :: GameState -> GameState
checkBulletsCollisions game@Game{..} =
    game {
        bullets =
            (map
                (\bull ->
                    if collisionWithAsteroids (bulLoc bull) 3 asteroids
                        then bull {bulAlive = False}
                        else bull
                )
                bullets
            )
    }
checkBulletsCollisions GameOver = GameOver


--check collisions for ship with other objects
checkShipCollision :: GameState -> GameState
checkShipCollision game@Game{..} =
    if wallCollision (shipLoc ship) 20
    || ((collisionWithAsteroids (shipLoc ship) 20 asteroids) && (not (shieldOn ship)))
    then
        game { ship = ship {shipAlive = False} }
    else
        game
checkShipCollision GameOver = GameOver


--check collisions for an object with all asteroids
collisionWithAsteroids :: Position -> Radius -> [Asteroid] -> Bool
collisionWithAsteroids _ _ [] = False
collisionWithAsteroids pos rad (a:as) =
    if (twoCirclesCollide pos rad (astLoc a) (astRad a))
        then True
        else collisionWithAsteroids pos rad as


--check collisions for an object with all bullets
collisionWithBullets :: Position -> Radius -> [Bullet] -> Bool
collisionWithBullets _ _ [] = False
collisionWithBullets pos rad (a:as) =
    if (twoCirclesCollide pos rad (bulLoc a) (bulRad a))
        then True
        else collisionWithBullets pos rad as


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
        dist = sqrt ((x1-x2)^2 + (y1-y2)^2)


moveAllObjects :: Time -> GameState -> GameState
moveAllObjects sec game@Game{..} =
    game {
        ship = move sec ship,
        asteroids = map (move sec) asteroids,
        bullets = map (move sec) bullets
    }
moveAllObjects _ GameOver = GameOver


delObjects :: GameState -> GameState
delObjects game@Game{..} =
    game {
        asteroids = (filter (\ast -> astAlive ast) asteroids),
        bullets = (filter (\bul -> bulAlive bul) bullets)
    }
delObjects GameOver = GameOver


updateObjects :: GameState -> GameState
updateObjects game@Game{..} =
    game {
        ship = updateShip ship
    }
updateObjects GameOver = GameOver


updateGame :: Time -> GameState -> GameState
updateGame sec game@Game{..} =
    (addAsteroid . delObjects . checkGameOver . checkCollisions . updateObjects . moveAllObjects sec)
    game {
        step = step + 1
    }
updateGame _ GameOver = GameOver
