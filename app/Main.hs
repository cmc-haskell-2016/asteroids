module Main (main) where

import Interface
import Rendering
import Game

import Graphics.Gloss.Interface.Pure.Game


background :: Color
background = makeColorI 25 25 112 0

fps :: Int
fps = 60


main :: IO ()
main = play window background fps initGame renderPic handleKeys updateGame
