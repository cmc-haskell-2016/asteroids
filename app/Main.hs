module Main (main) where

import Interface
import Rendering
import Game
import Types

import Graphics.Gloss.Interface.IO.Game

background :: Color
background = makeColorI 25 25 112 0

fps :: Int
fps = 60

getUpdatedGameState :: Time -> GameState -> IO GameState
getUpdatedGameState t gs = return (updateGame t gs)


main :: IO ()
main = do 
    game <- initUniverseIO
    playIO window background fps 
         (InGame game) renderPicIO handleKeys getUpdatedGameState


