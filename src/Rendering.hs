{-# LANGUAGE RecordWildCards #-}
module Rendering
(
    renderPicIO
) where

import Game
import GraphObject
import Ship

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
renderPic (InGame u@Universe{..}) =
    pictures
        ((draw ship) : (map draw asteroids) ++ (map draw ufos) ++ (map draw bullets) ++ [drawShield ship])
renderPic GameOver =
    scale 10 10 (pictures[
        translate 0 0 $
        color shipColor $
        rotate 0 $
        polygon [(10, -5), (0, 0), (0, 20)],
        translate 0 0 $
        color shipColor $
        rotate 0 $
        polygon [(-10, -5), (0, 0), (0, 20)],
        translate (-18) (-20) $
        scale 0.05 0.05 $
        color red $
        text "Game Over"])

renderPicIO :: GameState -> IO Picture
renderPicIO gs = do
    return $ renderPic gs