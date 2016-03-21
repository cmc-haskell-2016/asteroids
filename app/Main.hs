module Main where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)

background :: Color
background = dark (dark blue)

drawingShip :: Ship -> Picture
drawingShip ship = pictures[
	translate x y $
	color shipColor $
	rotate phi $
	polygon [(10, -5), (0, 0), (0, 20)],
	translate x y $
	color shipColor $
	rotate phi $
	polygon [(-10, -5), (0, 0), (0, 20)]]
	where
		(x, y) = shipLoc ship
		phi = shipAng ship


drawingAteroid :: Asteroid -> Picture
drawingAteroid ast = 
	translate x y $
	color (light black) $
	thickCircle rad rad 
	where
		(x, y) = astLoc ast
		rad = astSize ast

drawingBullet :: Bullet -> Picture
drawingBullet bull = 
	translate x y $
	color red $
	thickCircle 3 3
	where
		(x, y) = bulLoc bull

initialShip :: Ship
initialShip = Ship {
	shipLoc = (0, -100),
	shipAng = 0,
	shipVel = (0, 0),
	shipAlive = True
}

initialState :: GameState
initialState = Game initialShip [] []

drawing :: Picture
drawing = render initialState


render :: GameState -> Picture
render (Game s a b) = pictures ([(drawingShip s)] ++ (map drawingAteroid a) ++ (map drawingBullet b))



--deathState :: GameState
--deathState = Game {
--	shipLoc = (0, 0),
--	shipAng = 0,
--	shipVel = (0, 0),
--	bullets = []
--}

moveShip :: Float -> GameState -> GameState
moveShip sec (Game s a b) =  if vx == 0 && vy == 0 
					then Game s a b 
					else if (wallCollision (x, y) 20)
						then initialState
						else Game (s {shipLoc = (x1, y1), shipAng = (atan2 vx vy) * 180 / pi}) a b
	where
    (x, y) = shipLoc s
    (vx, vy) = shipVel s
    x1 = x + vx * sec
    y1 = y + vy * sec

	 

update :: Float -> GameState -> GameState
update seconds = moveShip seconds

wallCollision :: Position -> Radius -> Bool 
wallCollision pos rad = xCollision pos rad || yCollision pos rad

xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad = (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)

yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad = (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)

--wallDeath :: GameState -> GameState
--wallDeath game = if wallCollision (shipLoc game) 20 then deathState else game

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'w') Down _ _) (Game s a b) = Game (s {shipVel = (xv, yv + 50)}) a b where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'a') Down _ _) (Game s a b) = Game (s {shipVel = (xv - 50, yv)}) a b where (xv, yv) = shipVel s
handleKeys (EventKey (Char 's') Down _ _) (Game s a b) = Game (s {shipVel = (xv, yv - 50)}) a b where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'd') Down _ _) (Game s a b) = Game (s {shipVel = (xv + 50, yv)}) a b where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'w') Up _ _) (Game s a b) = Game (s {shipVel = (xv, yv - 50)}) a b where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'a') Up _ _) (Game s a b) = Game (s {shipVel = (xv + 50, yv)}) a b where (xv, yv) = shipVel s
handleKeys (EventKey (Char 's') Up _ _) (Game s a b) = Game (s {shipVel = (xv, yv + 50)}) a b where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'd') Up _ _) (Game s a b) = Game (s {shipVel = (xv - 50, yv)}) a b where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (s a b) = s {shipVel = (xv, yv + 50)} where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (s a b) = s {shipVel = (xv - 50, yv)} where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (s a b) = s {shipVel = (xv, yv - 50)} where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (s a b) = s {shipVel = (xv + 50, yv)} where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (s a b) = s {shipVel = (xv, yv - 50)} where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) (s a b) = s {shipVel = (xv + 50, yv)} where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyDown) Up _ _) (s a b) = game {shipVel = (xv, yv + 50)} where (xv, yv) = shipVel s
--handleKeys (EventKey (SpecialKey KeyRight) Up _ _) (s a b) = game {shipVel = (xv - 50, yv)} where (xv, yv) = shipVel s
handleKeys _ game = game 
	


main :: IO ()
main = play window background fps initialState render handleKeys update



