{-# LANGUAGE RecordWildCards #-}
module Rendering (renderPic) where

import Types
import GraphObjects
import Game

import Graphics.Gloss.Rendering
import Graphics.Gloss

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
        ((draw ship) : (map draw asteroids) ++ (map draw bullets) ++ [drawShield ship])
