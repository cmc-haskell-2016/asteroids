module Interface where

import Types
import Game

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game



offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 100


window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game t s a b) = Game t (s {shipAccel = True}) a b
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (Game t s a b) = Game t (s {shipAccel = False}) a b
--handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game t s a b) = Game t (s {shipVel = (shipVel s) + speedShip}) a b
--handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (Game t s a b) = Game t (s {shipVel = (shipVel s) - speedShip}) a b
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (Game t s a b) = Game t (s {rotation = (rotation s) - 5}) a b
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (Game t s a b) = Game t (s {rotation = (rotation s) + 5}) a b
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) (Game t s a b) = Game t (s {rotation = (rotation s) + 5}) a b
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) (Game t s a b) = Game t (s {rotation = (rotation s) - 5}) a b
handleKeys (EventKey (Char 's') Down _ _) (Game t s a b) = Game t (s {shieldOn = True}) a b
handleKeys (EventKey (Char 's') Up _ _) (Game t s a b) = Game t (s {shieldOn = False}) a b
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (Game t s a b) = Game t s a (initBullet(s) : b)
handleKeys _ game = game
