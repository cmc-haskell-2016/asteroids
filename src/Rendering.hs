{-# LANGUAGE RecordWildCards #-}
module Rendering (renderPic) where

import Types

import Graphics.Gloss.Rendering
import Graphics.Gloss


shipColor :: Color
shipColor = light (light red)


drawShip :: Ship -> Picture
drawShip s = if (shipAlive s)
    then
        pictures[
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(10, -5), (0, 0), (0, (shipSize s))],
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(-10, -5), (0, 0), (0, (shipSize s))]]
    else
        scale 10 10 (pictures[
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(10, -5), (0, 0), (0, (shipSize s))],
            translate x y $
            color shipColor $
            rotate phi $
            polygon [(-10, -5), (0, 0), (0, (shipSize s))],
            translate (-18) (-20) $
            scale 0.05 0.05 $
            color red $
            text "Game Over"])
    where
        (x, y) = shipLoc s
        phi = shipAng s


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
drawShield s =
    if shieldOn s then
        translate x y $
        color shipColor $
        circle (shieldRad s)
    else
        blank
    where
        (x, y) = shipLoc s


renderPic :: GameState -> Picture
renderPic game@Game{..} =
    pictures
        ((drawShip ship) : (map drawAsteroid asteroids) ++ (map drawBullet bullets) ++ [drawShield ship])
