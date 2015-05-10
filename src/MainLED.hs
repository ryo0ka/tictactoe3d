module MainLED where

	import Prelude hiding (init)
	import Data.Word
	import Control.Applicative
	import Control.Monad
	import Control.Monad.IO.Class(liftIO)
	import Reactive.Banana
	import Reactive.Banana.Frameworks
	import System.Hardware.Arduino
	import qualified Data.EnumMap.Lazy as E
	import Cube
	import TicTacToe
	import BiLed
	import BiLedCube

	digpin :: PinMode -> Word8 -> Arduino Pin
	digpin m n = do
		let p = digital n
		setPinMode p m
		return p

	bipins :: Word8 -> Word8 -> Arduino BiLed
	bipins p q = do
		p' <- digpin OUTPUT p
		q' <- digpin OUTPUT q
		return $ BiLed p' q'

	initLedCube :: Arduino BiLedCube
	initLedCube = do
		gb <- digpin OUTPUT 24
		gm <- digpin OUTPUT 23
		gt <- digpin OUTPUT 22
		nw <- bipins 28 29
		cw <- bipins 30 31
		sw <- bipins 32 33
		nc <- bipins 34 35
		cc <- bipins 36 37
		sc <- bipins 38 39
		ne <- bipins 40 41
		ce <- bipins 42 43
		se <- bipins 44 45

		let f  = E.fromList
		    gs = f [(P1, gb), (P2, gm), (P3, gt)]
		    b  = f [(P1, f [(P1, nw), (P2, nc), (P3, ne)]),
		            (P2, f [(P1, cw), (P2, cc), (P3, ce)]),
		            (P3, f [(P1, sw), (P2, sc), (P3, se)])]
		return $ BiLedCube gs b

	initButtons :: Arduino (Pin, Pin)
	initButtons = do
		m <- digpin INPUT 12
		e <- digpin INPUT 13
		return (m, e)

	data Game = Game { token :: Token, cube :: T3Cube }
	type Display = Cube Tril

	newGame :: Game
	newGame = Game True (init Nothing)

	main :: Arduino ()
	main = do
		c <- initLedCube
		(m, e) <- initButtons

		liftIO $ do
			let nwd :: Frameworks t => Moment t ()
			    nwd = do
					-- drawing each layer every 0.05 seconds:
					  -- make an event stream like [(0, ?), (0.05, ?), (0.10, ?), ...]
					  -- make a behavior of Pos
					  -- make a behavior of Cube Tril
					  -- for each item in the event stream, 
					    -- draw a layer of the Pos behavior 
					    -- inremenet the Pos behavior's value
					eLotation :: Event t Int -- whatever type
					eLocation = undefined

					bCurrentPos :: Behavior t Pos
					bCurrentPos = let
						inc :: Pos -> Pos
						inc P1 = P2
						inc P2 = P3
						inc P3 = P1
						draw :: Pos -> Behavior t Arduino () -- how to apply this?
						draw p = drawBoard c p <$> bDisplay
					in (\p _ -> inc p) <$> pure P1 <*> eLotation

					bDisplay :: Behavior t (Pos3 -> Tril)
					bDisplay = (!) <$> pure $ init Nothing

					-- treating a long push of a button as one input:
					  -- make an event stream of Bool (the button's state)
					  -- make a behavior of Bool (flag)
					  -- for each iterm in the event stream,
					    -- if it's True and the flag is False,
					      -- release True
					      -- switch the flag to True
					    -- otherwise,
					      -- switch the flag to False
					eButton :: Event t Bool -> Event t Bool
					eButton raw = 
						let
							bFlag :: Behavior t Bool
							bFlag = pure False
						in filterE (True ==) undefined -- how?

					-- moving a pivot every time the move button is pressed:
					  -- make an event stream of the button's input
					  -- make a behavior of Pos3
					  -- make a behavior of T3Cube
					  -- for each item in the event stream,
					    -- if it's True,
					      -- until the pos is available,
					        -- increment the Pos3 behavior's value
					      -- update the cube behavior's value
					eMoveInput :: Event t Bool
					eMoveInput = eButton undefined

					bPivot :: Behavior t Pos3
					bPivot =
						let
							inc' :: Pos3 -> Pos3
							inc' = undefined -- how?
							inc :: Pos3 -> Pos3
							inc p = case cube <$> bGame
						in (\p _ -> inc p) <$> pure (P1, P2, P3) <*> eMoveInput

					bGame :: Behavior t Game
					bGame = pure newGame

					-- applying a user input when he presses the end button:
					  -- make an event stream of the button's input
					  -- make a behavior of Pos3
					  -- make a behavior of Game
					  -- for each item in the event stream,
					    -- if it's True,
					      -- if he wins,
					      	-- end the game
					      -- otherwise,
					        -- update the Game behavior's value

					return ()

			nw <- compile nwd
			actuate nw