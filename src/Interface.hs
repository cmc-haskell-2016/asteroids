{-# LANGUAGE RecordWildCards #-}
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
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game@Game{..} =
    game {
        ship = ship {shipAccel = True}
    }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game@Game{..} =
    game {
        ship = ship {shipAccel = False}
    }
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game@Game{..} =
    game {
        ship = ship {rotation = (rotation ship) - 5}
    }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game@Game{..} =
    game {
        ship = ship {rotation = (rotation ship) + 5}
    }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game@Game{..} =
    game {
        ship = ship {rotation = (rotation ship) + 5}
    }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game@Game{..} =
    game {
        ship = ship {rotation = (rotation ship) - 5}
    }
handleKeys (EventKey (Char 's') Down _ _) game@Game{..} =
    game {
        ship = ship {shieldOn = True}
    }
handleKeys (EventKey (Char 's') Up _ _) game@Game{..} =
    game {
        ship = ship {shieldOn = False}
    }
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game@Game{..} =
    game {
        bullets = (initBullet ship) : bullets
    }
handleKeys _ game = game
--handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = Game t ((ship game) {shipVel = (shipVel (ship game)) + speedShip}) a b
--handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = Game t ((ship game) {shipVel = (shipVel (ship game)) - speedShip}) a b
