module Main where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


window :: Display
window = InWindow "ASTEROID BATTLE by Team Stolyarov" (width, height) (offsetX, offsetY)


drawingShip :: Ship -> Picture
drawingShip ship = if shipAlive ship
	then
	    pictures[
		translate x y $
		color shipColor $
		rotate phi $
		polygon [(10, -5), (0, 0), (0, 20)],
		translate x y $
		color shipColor $
		rotate phi $
		polygon [(-10, -5), (0, 0), (0, 20)]]
	else
		scale 10 10 (pictures[
		translate x y $
		color shipColor $
		rotate phi $
		polygon [(10, -5), (0, 0), (0, 20)],
		translate x y $
		color shipColor $
		rotate phi $
		polygon [(-10, -5), (0, 0), (0, 20)],
		translate (-18) (-20) $
		scale 0.05 0.05 $
		color red $
		text "Game Over"])
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
	if bulAlive bull then
		translate x y $
		color red $
		thickCircle 1.2 3
	else blank
	where
		(x, y) = bulLoc bull

initialShip :: Ship
initialShip = Ship {
	shipLoc = (0, 0),
	shipAng = 0,
	rotation = 0,
	shipVel = 0,
	shipAlive = True
}

deathShip :: Ship
deathShip = Ship {
	shipLoc = (0, 0),
	shipAng = 0,
	rotation = 0,
	shipVel = 0,
	shipAlive = False
}

initialState :: GameState
initialState = Game initialShip [] []

deathState :: GameState
deathState = Game deathShip [] []

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
moveShip sec (Game s a b) = if ((not (shipAlive s)) || (wallCollision (x, y) 20))
								then deathState
							else Game (s {shipAng = newAng, shipLoc = (x1, y1)}) a b
	where
    (x, y) = shipLoc s
    v = shipVel s
    newAng = (shipAng s) + (rotation s)
    x1 = x + v* (sin (newAng*pi/180)) * sec
    y1 = y + v* (cos (newAng*pi/180)) * sec

moveBullets :: Time -> GameState -> GameState
moveBullets sec (Game s a b) =Game s a (map (moveBull sec) b)

moveBull :: Time -> Bullet -> Bullet
moveBull sec bull = if (wallCollision (x,y) 3)
					then bull {bulAlive = False}
					else bull {bulLoc = (x1, y1)}
	where
	(x, y) = bulLoc bull
	(vx, vy) = bulVel bull
	x1 = x + vx * sec
	y1 = y + vy * sec
--asteroidCollision :: Position -> Radius -> [Asteroid] -> Bool
--asteroidCollision (x, y) rad ast = foldl ()

update :: Time -> GameState -> GameState
update seconds game = moveShip seconds (moveBullets seconds game)

wallCollision :: Position -> Radius -> Bool
wallCollision pos rad = xCollision pos rad || yCollision pos rad

xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad = (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)

yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad = (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)

--wallDeath :: GameState -> GameState
--wallDeath game = if wallCollision (shipLoc game) 20 then deathState else game

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game s a b) = Game (s {shipVel = (shipVel s) + 70}) a b
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (Game s a b) = Game (s {shipVel = (shipVel s) - 70}) a b
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (Game s a b) = Game (s {rotation = (rotation s) - 5}) a b
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (Game s a b) = Game (s {rotation = (rotation s) + 5}) a b
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) (Game s a b) = Game (s {rotation = (rotation s) + 5}) a b
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) (Game s a b) = Game (s {rotation = (rotation s) - 5}) a b
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (Game s a b) = Game s a ((Bullet {bulLoc = (x, y), bulAng = ang, bulAlive = True, bulVel = velang}) : b)
	 where
	 	(x, y) = shipLoc s
	 	ang = shipAng s
	 	yvel = cos ((shipAng s) * pi / 180)
	 	xvel = sin ((shipAng s) * pi / 180)
	 	norm = sqrt (xvel * xvel + yvel * yvel)
	 	velang = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)
handleKeys _ game = game



main :: IO ()
main = play window background fps initialState render handleKeys update



