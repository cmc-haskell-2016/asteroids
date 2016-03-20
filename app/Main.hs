module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
--import Graphics.Gloss.Interface.IO.Game

type Radius = Float
type Position = (Float, Float)
type Degree = Float

width :: Int
width = 700

height :: Int
height = 500

offsetX :: Int
offsetX = 500

offsetY :: Int
offsetY = 250

fps :: Int
fps = 60

window :: Display
window = InWindow "Asteroids" (width, height) (offsetX, offsetY)

background :: Color
background = makeColorI 75 75 186 0

shipColor :: Color
shipColor = black

drawingShip :: Position -> Degree -> Picture
drawingShip (x, y) phi = pictures[
	translate x y $
	color shipColor $
	rotate phi $
	polygon [(10, -5), (0, 0), (0, 20)],
	translate x y $
	color shipColor $
	rotate phi $
	polygon [(-10, -5), (0, 0), (0, 20)]]

--drawing :: Picture
--drawing = pictures[
--	drawingShip]
drawing :: Picture
drawing = render initialState


data GameState = Game {
	shipLoc :: Position,
	shipAng :: Degree,
	shipVel :: (Float, Float)
} deriving Show

render :: GameState -> Picture
render game = pictures [(drawingShip (shipLoc game) (shipAng game))]

initialState :: GameState
initialState = Game {
	shipLoc = (0, -100),
	shipAng = 0,
	shipVel = (0, 50)
}

deathState :: GameState
deathState = Game {
	shipLoc = (0, 0),
	shipAng = 0,
	shipVel = (0, 0)
}

moveShip :: Float -> GameState -> GameState
moveShip sec game = game {shipLoc = (x1, y1)}
	where
    (x, y) = shipLoc game
    (vx, vy) = shipVel game
    x1 = x + vx * sec
    y1 = y + vy * sec 

frame :: Float -> Picture
frame seconds = render $ moveShip seconds initialState

update :: ViewPort -> Float -> GameState -> GameState
update _ seconds = wallDeath . moveShip seconds

wallCollision :: Position -> Radius -> Bool 
wallCollision pos rad = xCollision pos rad || yCollision pos rad

xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad = (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)

yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad = (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)

wallDeath :: GameState -> GameState
wallDeath game = if wallCollision (shipLoc game) 20 then deathState else game


main :: IO ()
main = simulate window background fps initialState render update



