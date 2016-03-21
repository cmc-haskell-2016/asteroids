module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
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

drawingShip :: GameState -> Picture
drawingShip game = pictures[
	translate x y $
	color shipColor $
	rotate phi $
	polygon [(10, -5), (0, 0), (0, 20)],
	translate x y $
	color shipColor $
	rotate phi $
	polygon [(-10, -5), (0, 0), (0, 20)]]
	where
		(x, y) = shipLoc game
		(xv, yv) = shipVel game
		phi = shipAng game
		--phi = (atan2 xv yv) * 180 / pi

--drawing :: Picture
--drawing = pictures[
--	drawingShip]
drawing :: Picture
drawing = render initialState


data GameState = Game {
	shipLoc :: Position,
	shipAng :: Float,
	shipVel :: (Float, Float)
} deriving Show

render :: GameState -> Picture
render game = pictures [(drawingShip game)]

initialState :: GameState
initialState = Game {
	shipLoc = (0, -100),
	shipAng = 0,
	shipVel = (0, 0)
}

deathState :: GameState
deathState = Game {
	shipLoc = (0, 0),
	shipAng = 0,
	shipVel = (0, 0)
}

moveShip :: Float -> GameState -> GameState
moveShip sec game = if vx == 0 && vy == 0 
					then game 
					else game {shipLoc = (x1, y1), shipAng = (atan2 vx vy) * 180 / pi}
	where
    (x, y) = shipLoc game
    (vx, vy) = shipVel game
    x1 = x + vx * sec
    y1 = y + vy * sec 

frame :: Float -> Picture
frame seconds = render $ moveShip seconds initialState

update :: Float -> GameState -> GameState
update seconds = wallDeath . moveShip seconds

wallCollision :: Position -> Radius -> Bool 
wallCollision pos rad = xCollision pos rad || yCollision pos rad

xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad = (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)

yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad = (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)

wallDeath :: GameState -> GameState
wallDeath game = if wallCollision (shipLoc game) 20 then deathState else game

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'w') Down _ _ ) game = game {shipVel = (xv, yv + 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (Char 'a') Down _ _ ) game = game {shipVel = (xv - 50, yv)} where (xv, yv) = shipVel game
handleKeys (EventKey (Char 's') Down _ _ ) game = game {shipVel = (xv, yv - 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (Char 'd') Down _ _ ) game = game {shipVel = (xv + 50, yv)} where (xv, yv) = shipVel game
handleKeys (EventKey (Char 'w') Up _ _ ) game = game {shipVel = (xv, yv - 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (Char 'a') Up _ _ ) game = game {shipVel = (xv + 50, yv)} where (xv, yv) = shipVel game
handleKeys (EventKey (Char 's') Up _ _ ) game = game {shipVel = (xv, yv + 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (Char 'd') Up _ _ ) game = game {shipVel = (xv - 50, yv)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyUp) Down _ _ ) game = game {shipVel = (xv, yv + 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _ ) game = game {shipVel = (xv - 50, yv)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyDown) Down _ _ ) game = game {shipVel = (xv, yv - 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _ ) game = game {shipVel = (xv + 50, yv)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyUp) Up _ _ ) game = game {shipVel = (xv, yv - 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _ ) game = game {shipVel = (xv + 50, yv)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyDown) Up _ _ ) game = game {shipVel = (xv, yv + 50)} where (xv, yv) = shipVel game
handleKeys (EventKey (SpecialKey KeyRight) Up _ _ ) game = game {shipVel = (xv - 50, yv)} where (xv, yv) = shipVel game
handleKeys _ game = game 
	


main :: IO ()
main = play window background fps initialState render handleKeys update



