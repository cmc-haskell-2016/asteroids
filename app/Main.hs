module Main (main) where

import Types
import Interface
import Rendering
import Game

import Graphics.Gloss.Interface.Pure.Game


main :: IO ()
main = play window background fps initialState renderPic handleKeys updateGame
