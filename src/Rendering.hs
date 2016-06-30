{-# LANGUAGE RecordWildCards #-}
module Rendering
(
    renderPicIO
) where

import Game
import GraphObject
import Ship
import Universe
import ClientSide

import Graphics.Gloss.Rendering
import Graphics.Gloss
import Control.Concurrent.STM


drawShield :: Ship -> Picture
drawShield s =
    if shieldOn s then
        translate x y $
        color (shipColors (shipColor s)) $
        circle (shieldRad s)
    else
        blank
    where
        (x, y) = shipLoc s


renderPic :: GameState -> Picture
renderPic (Waiting n) =
    scale 5 5 $
    pictures[
        translate 0 0 $
        scale 0.05 0.05 $
        color red $
        text ("Waiting players: " ++ (show n))]
renderPic (InGame (_u@Universe{..}:_)) =
    pictures
        ((map draw ships) ++ (map draw asteroids) ++ (map draw bullets) ++ (map drawShield ships))
renderPic GameOver =
    scale 10 10 (pictures[
        translate 0 0 $
        color (shipColors 1) $
        rotate 0 $
        polygon [(10, -5), (0, 0), (0, 20)],
        translate 0 0 $
        color (shipColors 1) $
        rotate 0 $
        polygon [(-10, -5), (0, 0), (0, 20)],
        translate (-18) (-20) $
        scale 0.05 0.05 $
        color red $
        text "Game Over"])
renderPic _ = pictures [text "foo"]


renderPicIO :: ClientState -> IO Picture
renderPicIO cs = do
    gs <- readTVarIO (game cs)
    return $ renderPic gs