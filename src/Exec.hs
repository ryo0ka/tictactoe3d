-- Defines Tic-Tac-Toe game execution on an LED cube.
module Exec where
	import Control.Monad
	import Control.Monad.IO.Class
	import Data.Word
	import Data.Functor
	import Data.Traversable as T
	import Data.Foldable as F
	import Data.Tuple.Homogenous
	import System.Hardware.Arduino
	import Vector3 as V
	import BiLedCube
	import TicTacToe
	import ArduinoUtil
	import Util (thenDo, thenJust)

	-- Represents required components for the game.
	type Src = (Cube, Button, Button)

	-- Moves a cursor forward.
	succCursor :: Game -> Int -> Int
	succCursor (Done _ _) i = i
	succCursor (Game (_, b) _) i = succ' b i where
		succ' board i =
			let	next = succ i
			in case board V.! i3 next of
				Just _ -> succ' board next
				Nothing -> next

	-- Proceeds the game with the given game/cursor's states.
	succGame :: (Game, Int) -> (Game, Int)
	succGame (gameP, cursorP) =
		let	gameC = playGame cursorP gameP
			cursorC = (not (done gameC) `thenDo` succCursor gameC) cursorP
		in (gameC, cursorC)

	-- Retrieves a function that takes in a location
	-- and returns a display color according to
	-- the given game's and the given cursor's state.
	reflect :: Game -> Int -> (I3 -> Maybe Bool)
	reflect (Done t cs) _ c =
		(F.elem c cs) `thenJust` t
	reflect (Game (_, b) t) i c
		| (i3 i) == c = Just t
		| otherwise = b V.! c

	-- Executes a game with the given
	-- positions of Arduino components.
	exec :: Src -> Arduino ()
	exec (cube, move, kick) =
		loop cube move kick newGame 0 LA where
			loop cube moveP kickP gameP cursorP layerP = do
				(moved,  moveC) <- readButton moveP
				(kicked, kickC) <- readButton kickP
				let (gameC, cursorC) =
					let cursorC' = (moved `thenDo` succCursor gameP) cursorP
					in (kicked `thenDo` succGame) (gameP, cursorC')
				drawCube cube layerP (reflect gameC cursorC)
				loop cube moveC kickC gameC cursorC (succLayer layerP)

	-- Hardcodes my Arduino's preferences
	mySrc :: Arduino Src
	mySrc = do
		gnds <- T.sequence $ digpin OUTPUT <$> Tuple3 (
			25,
			22,
			23)
		leds <- T.sequence $ bipin <$> Tuple9 (
			(26, 27),
			(30, 31),
			(34, 35),
			(38, 39),
			(40, 41),
			(46, 47),
			(48, 49),
			(50, 51),
			(52, 53))
		Tuple2 (m, k) <- T.sequence $ initButton <$> Tuple2 (
			12,
			13)
		return (Cube gnds leds, m, k)

	-- Hardcodes my Arduino's preferences
	withMyArduino :: Arduino () -> IO ()
	withMyArduino = withArduino False "COM3"

	-- Executes a game with the hardcoded preferences
	main :: IO ()
	main = withMyArduino $ mySrc >>= exec

	-- Tests out the move/kick buttons of the given preference
	testButtons :: Src -> Arduino ()
	testButtons (c, m, k) = do
		loop c m 0 k False LA where
			loop cube moveP curP kickP colP lyrP = do
				(move, moveC) <- readButton moveP
				(kick, kickC) <- readButton kickP
				let	curC = (move `thenDo` succ) curP
					colC = (kick `thenDo` not ) colP
				drawCube cube lyrP $ \c -> (c == i3 curC) `thenJust` colC
				loop cube moveC curC kickC colC (succLayer lyrP)

	-- Tests out the LED cube of the 
	-- given preference with the specified color
	testLamps :: Maybe Bool -> Src -> Arduino ()
	testLamps b (Cube _ leds, _, _) = drawLayer leds $ const b
