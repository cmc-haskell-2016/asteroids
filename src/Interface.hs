{-# LANGUAGE RecordWildCards #-}
module Interface where

import Game
import Ship
import Bullet

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game


offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 100


window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)

handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {shipAccel = True}
    })
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {shipAccel = False}
    })
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {rotation = (rotation ship) - 5}
    })
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {rotation = (rotation ship) + 5}
    })
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {rotation = (rotation ship) + 5}
    })
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {rotation = (rotation ship) - 5}
    })
handleKeys (EventKey (Char 's') Down _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {shieldOn = True}
    })
handleKeys (EventKey (Char 's') Up _ _) (InGame u@Universe{..}) = return
    (InGame u {
        ship = ship {shieldOn = False}
    })
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (InGame u@Universe{..}) = return 
    (InGame u {
        bullets = (initBullet ship) : bullets
    })
handleKeys _ gs = return gs
