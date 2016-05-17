{-# LANGUAGE RecordWildCards #-}
module Interface where

import Game

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


data Action =
    EnableAcceleration
    | DisableAcceleration
    | RotateLeft
    | RotateRight
    | EnableShield
    | DisableShield
    | Shoot

offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 100

window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)


sendAction :: Action -> GameState-> IO GameState
-- TODO
sendAction action gs = return gs

handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) = sendAction EnableAcceleration
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) = sendAction DisableAcceleration
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) = sendAction RotateLeft
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) = sendAction RotateRight
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) = sendAction RotateRight
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) = sendAction RotateLeft
handleKeys (EventKey (Char 's') Down _ _) = sendAction EnableShield
handleKeys (EventKey (Char 's') Up _ _) = sendAction DisableShield
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) = sendAction Shoot
handleKeys _ = return
