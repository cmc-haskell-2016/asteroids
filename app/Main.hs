module Main (main) where

import Interface
import Rendering
import Game

import Graphics.Gloss.Interface.Pure.Game
import System.Random


background :: Color
background = makeColorI 25 25 112 0

fps :: Int
fps = 60


main :: IO ()
main = do 
    gen <- getStdGen
    randLoc <- return (randomRs ((-200)::Float, 200::Float) gen)
    randVel <- return (randomRs ((-70)::Float, 70::Float) gen)
    randRad <- return (randomRs (10::Float, 50::Float) gen)
    play window background fps 
         (InGame (initUniverse randLoc randVel randRad)) renderPic handleKeys updateGame


