module Main where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Rendering
import System.Random


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
	color black $
	circleSolid rad
	where
		(x, y) = astLoc ast
		rad = astSize ast
{-
drawingSpace :: Picture
drawingSpace = do
	let p <- loadBMP "space.bmp"
-}

drawingBullet :: Bullet -> Picture
drawingBullet bull =
	if bulAlive bull then
		translate x y $
		color red $
		circleSolid 3
	else blank
	where
		(x, y) = bulLoc bull

initialShip :: Ship
initialShip = Ship {
	shipLoc = (0, -100),
	shipAng = 0,
	shipVel = (0, 0),
	shipAlive = True
}

deathShip :: Ship
deathShip = Ship {
	shipLoc = (0, 0),
	shipAng = 0,
	shipVel = (0, 0),
	shipAlive = False
}

initialState :: GameState
initialState = Game 0 initialShip [] []

deathState :: GameState
deathState = Game 0 deathShip [] []

renderPic :: GameState -> Picture
renderPic (Game _ s a b) = pictures ([(drawingShip s)] ++ (map drawingAteroid a) ++ (map drawingBullet b))
{-}
addAsteroid :: GameState -> GameState
addAsteroid (Game 180 s ast b ) = Game 180 s (a : ast) b
	where
		a = genAsteroid (mkStdGen (length ast))
addAsteroid game = game
-}
addAsteroid :: GameState -> GameState
addAsteroid (Game 180 s ast b) = Game 180 s (a : ast) b
	where
		genShipX = mkStdGen (round (fst (shipLoc s)))
		randPos = take 2 (randomRs ((-250)::Float, 250::Float) genShipX)
		randLoc = (head randPos, head (tail randPos))
		genShipY = mkStdGen (round (snd (shipLoc s)))
		randSpeed = take 2 (randomRs ((-70)::Float, 70::Float) genShipY)
		randVel = (head randSpeed, head (tail randSpeed))
		genAst = mkStdGen (length ast)
		randInt = take 1 (randomRs (10::Float, 50::Float) genAst)
		randRad = head randInt
		a = Asteroid {astLoc = randLoc, astAng = 0, astSize = randRad, astAlive = True, astVel = randVel}
addAsteroid game = game


moveShip :: Time -> GameState -> GameState
moveShip sec (Game t s a b) = if (not (shipAlive s))
					then deathState
					else if vx == 0 && vy == 0
					then Game t s a b
					else if (wallCollision (x, y) 20)
						then deathState
						else Game t (s {shipLoc = (x1, y1), shipAng = (atan2 vx vy) * 180 / pi}) a b
						where
						(x, y) = shipLoc s
						(vx, vy) = shipVel s
						x1 = x + vx * sec
						y1 = y + vy * sec

moveBullets :: Time -> GameState -> GameState
moveBullets sec (Game t s a b) =Game t s a (map (moveBull sec) b)

moveBull :: Time -> Bullet -> Bullet
moveBull sec bull = if (wallCollision (x,y) 3)
					then bull {bulAlive = False}
					else bull {bulLoc = (x1, y1)}
	where
	(x, y) = bulLoc bull
	(vx, vy) = bulVel bull
	x1 = x + vx * sec
	y1 = y + vy * sec

moveAsteroids :: Time -> GameState -> GameState
moveAsteroids sec (Game t s a b) =Game t s (map (moveAst sec) a) b

moveAst :: Time -> Asteroid -> Asteroid
moveAst sec ast = if (wallCollision (x,y) ((astSize ast) / 2))
					then ast {astAlive = False}
					else ast {astLoc = (x1, y1)}
	where
	(x, y) = astLoc ast
	(vx, vy) = astVel ast
	x1 = x + vx * sec
	y1 = y + vy * sec
--asteroidCollision :: Position -> Radius -> [Asteroid] -> Bool
--asteroidCollision (x, y) rad ast = foldl ()

updateGame :: Time -> GameState -> GameState
updateGame sec (Game t s a b) = addAsteroid (moveObjects sec (Game (updateStep t) s a b))

updateStep :: Step -> Step
updateStep 181 = 0
updateStep t = t + 1

moveObjects :: Time -> GameState -> GameState
moveObjects sec game = moveShip sec (moveBullets sec (moveAsteroids sec game))

wallCollision :: Position -> Radius -> Bool
wallCollision pos rad = xCollision pos rad || yCollision pos rad

xCollision :: Position -> Radius -> Bool
xCollision (x, _) rad = (x + rad >= fromIntegral width/2) || (x - rad <= -fromIntegral width/2)

yCollision :: Position -> Radius -> Bool
yCollision (_, y) rad = (y + rad >= fromIntegral height/2) || (y - rad <= -fromIntegral height/2)

--wallDeath :: GameState -> GameState
--wallDeath game = if wallCollision (shipLoc game) 20 then deathState else game



handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'w') Down _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv + 50)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'a') Down _ _) (Game t s a b) = Game t (s {shipVel = (xv - 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (Char 's') Down _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv - 50)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'd') Down _ _) (Game t s a b) = Game t (s {shipVel = (xv + 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'w') Up _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv - 50)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'a') Up _ _) (Game t s a b) = Game t (s {shipVel = (xv + 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (Char 's') Up _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv + 50)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (Char 'd') Up _ _) (Game t s a b) = Game t (s {shipVel = (xv - 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv + 50)})a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) (Game t s a b) = Game t (s {shipVel = (xv - 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv - 50)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) (Game t s a b) = Game t (s {shipVel = (xv + 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv - 50)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) (Game t s a b) = Game t (s {shipVel = (xv + 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) (Game t s a b) = Game t (s {shipVel = (xv, yv + 50)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) (Game t s a b) = Game t (s {shipVel = (xv - 50, yv)}) a b
	where (xv, yv) = shipVel s
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (Game t s a b) = Game t s a ((Bullet {bulLoc = (x, y), bulAng = ang, bulAlive = True, bulVel = velang}) : b)
	 where
		(x, y) = shipLoc s
		ang = shipAng s
		yvel = cos ((shipAng s) * pi / 180)
		xvel = sin ((shipAng s) * pi / 180)
		norm = sqrt (xvel * xvel + yvel * yvel)
		velang = (xvel /norm * bulletSpeed, yvel /norm * bulletSpeed)
handleKeys _ game = game



main :: IO ()
main = play window background fps initialState renderPic handleKeys updateGame



