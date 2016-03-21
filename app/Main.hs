module Main where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)

background :: Color
background = dark (dark blue)

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
		phi = shipAng game

drawing :: Picture
drawing = render initialState


render :: GameState -> Picture
render game = pictures [(drawingShip game)]

initialState :: GameState
initialState = Game {
	shipLoc = (0, -100),
	shipAng = 0,
	shipVel = (0, 0),
	bullets = []
}

deathState :: GameState
deathState = Game {
	shipLoc = (0, 0),
	shipAng = 0,
	shipVel = (0, 0),
	bullets = []
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

--moveBullets :: Float -> GameState -> GameState
--moveBullets sec game = map (\bull -> bull {bulLoc = ((fst (bulLoc bull)) + 100 * sec, (snd (bulLoc bull)) + 100 * sec)} 
	 

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



