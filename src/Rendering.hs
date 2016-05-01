module Rendering (renderPic) where

import Types

import Graphics.Gloss.Rendering
import Graphics.Gloss


drawShip :: Ship -> Picture
drawShip ship = if shipAlive ship
    then
        pictures[
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(10, -5), (0, 0), (0, mainShipSize)],
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(-10, -5), (0, 0), (0, mainShipSize)]]
    else
        scale 10 10 (pictures[
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(10, -5), (0, 0), (0, mainShipSize)],
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(-10, -5), (0, 0), (0, mainShipSize)],
            translate (-18) (-20) $
            scale 0.05 0.05 $
            color red $
            text "Game Over"])
    where
        (x, y) = shipLoc ship
        phi = shipAng ship


drawAsteroid :: Asteroid -> Picture
drawAsteroid ast =
        translate x y $
        color black $
        circleSolid rad
    where
        (x, y) = astLoc ast
        rad = astSize ast


drawBullet :: Bullet -> Picture
drawBullet bull =
        translate x y $
        color red $
        circleSolid 3
    where
        (x, y) = bulLoc bull


drawShield :: Ship -> Picture
drawShield ship =
    if shieldOn ship then
        translate x y $
        color shipColor $
        circle shieldRad
    else
        blank
    where
        (x, y) = shipLoc ship


renderPic :: GameState -> Picture
renderPic (Game _ s a b) =
    pictures
        ((drawShip s) : (map drawAsteroid a) ++ (map drawBullet b) ++ [drawShield s])
