{-# LANGUAGE RecordWildCards #-}
module Interface where

import Game
import ClientSide

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.Exit


data Action =
    EnableAcceleration
    | DisableAcceleration
    | RotateLeft
    | RotateRight
    | EnableShield
    | DisableShield
    | Shoot deriving (Read, Show)

instance WebSocketsData Action where
    fromLazyByteString = read . BL8.unpack
    toLazyByteString   = BL8.pack . show


offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 100

window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)


sendAction :: Action -> ClientState-> IO ClientState
-- TODO
sendAction action cs = do
    sendBinaryData (conn cs) action
    return cs


handleKeys :: Event -> ClientState -> IO ClientState
handleKeys (EventKey (SpecialKey KeyEsc) Down _ _) = return exitSuccess
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
