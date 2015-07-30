module Exec (exec) where
	import Data.IORef
	import Data.Word
	import Data.Traversable as T
	import Control.Monad as M
	import Control.Monad.IO.Class
	import Control.Applicative
	import Data.Tuple.Homogenous
	import System.Hardware.Arduino
	import Reactive.Banana
	import Reactive.Banana.Frameworks
	import BiLed
	import BiLedCube as C
	import TicTacToe as T
	import Util

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

	-- initial -> stream a -> stream (previous, current)
	progress :: a -> Event t a -> Event t (a, a)
	progress d es = (,) <$> stepper d es <@> es

	-- ((a, b), (c, d)) -> ((a, c), (b, d))
	sequence2 :: ((a, a), (a, a)) -> Tuple2 (Tuple2 a)
	sequence2 = T.sequence . fmap Tuple2 . Tuple2

	-- TODO succCoord :: Coord -> Coord
	-- carrying Int as Coord doesn't make sense

	int2coord :: Int -> Coord
	int2coord i = t $ base 3 i
		where t (x:y:z:_) = (x, y, z)

	-- (previous, current) -> state
	pressed :: (Bool, Bool) -> Bool
	pressed (p, c) = not p && c

	data Game = Game Board Team | Done Team [Coord]

	newGame :: Game
	newGame = Game (T.board 3 $ const Nothing) True

	kick :: Game -> Int -> Game
	kick g @ (Done _ _) _ = g
	kick g @ (Game b t) c =
		case play b t (int2coord c) of
			Just (Left cs)  -> Done t cs
			Just (Right b') -> Game b' (not t)
			Nothing         -> g

	reflect :: Game -> Int -> (Coord -> Maybe Bool)
	reflect (Done t cs) _ c =
		(elem c cs) `thenJust` t
	reflect (Game (_, b) t) i c
		| (int2coord i) == c = Just t -- cursor location
		| otherwise = T.get b c

	exec :: Arduino ()
	exec = do
		gnds <- T.sequence $ Tuple3 (
			digpin OUTPUT 24,
			digpin OUTPUT 23,
			digpin OUTPUT 22)
		leds <- T.sequence $ Tuple9 (
			bipins 28 29,
			bipins 30 31,
			bipins 32 33,
			bipins 34 35,
			bipins 36 37,
			bipins 38 39,
			bipins 40 41,
			bipins 42 43,
			bipins 44 45)
		mover  <- digpin INPUT 46
		kicker <- digpin INPUT 48

		let	cube = C.Cube gnds leds
		mapGame <- liftIO $ newIORef $ const Nothing
		layer   <- liftIO $ newIORef $ C.LA

		let	desc add = do
				eBtns <- fromAddHandler add
				let	-- eMK is `Event (Bool, Bool)` where
					-- the first bool is the MOVE button's state
					-- the second one is of the KICK button's
					eMK = fmap f $ progress (False, False) $ eBtns where
						f = fmap (pressed . untuple2) . sequence2
					-- bGame is `Behavior (Game, Int)` that
					-- will be mapped to the LED cube by `reflect`
					bGame = accumB (newGame, 0) $ f <$> eMK where
						f (Tuple2 (m, k)) (g, i) =
							let	i' = if m then succ i else i
								g' = if k then kick g i' else g
							in (g', i')
				reactimate undefined -- TODO modify `mapGame` by `reflect`

		fire <- liftIO $ do
			(add, handler) <- newAddHandler
			compile (desc add) >>= actuate
			return handler
		forever $ do
			move <- digitalRead mover
			kick <- digitalRead kicker
			liftIO $ fire (move, kick)
			m <- liftIO $ readIORef mapGame
			l <- liftIO $ readIORef layer
			C.drawCube cube l m
			liftIO $ modifyIORef layer C.succLayer