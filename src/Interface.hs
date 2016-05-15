{-# LANGUAGE RecordWildCards #-}
module Interface where

import Game

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 100

window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)


serverShipAccel :: GameState -> GameState
serverShipAccel GameOver = GameOver
serverShipAccel game@Game{..} = do
    -- send this to server
    -- game {
    --     ship = ship {shipAccel = True}
    -- }
    game

serverShipNoAccel :: GameState -> GameState
serverShipNoAccel GameOver = GameOver
serverShipNoAccel game@Game{..} = do
    -- send this to server
    -- game {
    --     ship = ship {shipAccel = False}
    -- }
    game

serverShipRotateLeft :: GameState -> GameState
serverShipRotateLeft GameOver = GameOver
serverShipRotateLeft game@Game{..} =
    -- send this to server
    -- game {
    --     ship = ship {rotation = (rotation ship) - 5}
    -- }
    game

serverShipRotateRight :: GameState -> GameState
serverShipRotateRight GameOver = GameOver
serverShipRotateRight game@Game{..} =
    -- send this to server
    -- game {
    --     ship = ship {rotation = (rotation ship) + 5}
    -- }
    game

serverShieldOn :: GameState -> GameState
serverShieldOn GameOver = GameOver
serverShieldOn game@Game{..} =
    -- send this to server
    -- game {
    --     ship = ship {shieldOn = True}
    -- }
    game

serverShieldOff :: GameState -> GameState
serverShieldOff GameOver = GameOver
serverShieldOff game@Game{..} =
    -- send this to server
    -- game {
    --     ship = ship {shieldOn = False}
    -- }
    game

serverShoot :: GameState -> GameState
serverShoot GameOver = GameOver
serverShoot game@Game{..} =
    -- send this to server
    -- game {
    --     bullets = (initBullet ship) : bullets
    -- }
    game

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
    serverShipAccel game
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game =
    serverShipNoAccel game
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game =
    serverShipRotateLeft game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game =
    serverShipRotateRight game
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game =
    serverShipRotateRight game
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game =
    serverShipRotateLeft game
handleKeys (EventKey (Char 's') Down _ _) game =
    serverShieldOn game
handleKeys (EventKey (Char 's') Up _ _) game =
    serverShieldOff game
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game =
    serverShoot game
handleKeys _ game = game
>>>>>>> Added WebSockets
