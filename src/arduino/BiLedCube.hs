module BiLedCube where

	import BiLedOut
	import Util
	import Direction
	import World
	import WorldDirected
	import TicTacToe
	import System.Hardware.Arduino
	import qualified Data.EnumMap.Lazy as E
	import qualified Data.Foldable as F

	type BiLedCube = World BiLedOut
	type BiLedCubeGnd = E.EnumMap Position Pin

	biled :: Maybe Bool -> Location -> BiLedCube -> Arduino ()
	biled t l c = biLedWrite (getW l c) t

	-- Runs all the ground pins once.
	-- Execute constantly, like every 10 milliseconds.
	runGnds :: BiLedCubeGnd -> Arduino ()
	runGnds c = F.mapM_ f allEnum where
		f pos = F.mapM_ f' (E.assocs c) where
			f' (pos', pin) = digitalWrite pin (pos == pos')

{-
	type BiLeds = Dim2 E.EnumMap Position BiLedOut
	type BiLedCube = E.EnumMap Position (Pin, BiLeds)

	layerAt :: BiLedCube -> Vertical -> BiLeds
	layerAt c v = c E.! v

	gndAt :: BiLedCube -> Vertical -> Pin
	gndAt c v = fst(layerAt c v)

	outAt :: BiLedCube -> Vertical -> Horizon -> BiLedOut
	outAt c v h = snd(layerAt c v) E.! h

	-- Runs through all the ground pins once.
	-- Execute every 10 milliseconds or shorter.
	runGnds :: BiLedCube -> Arduino()
	runGnds c = mapM_ f [minBound ..] where
		f v = mapM_ g (E.assocs c) where
			g (w, (p, _)) = digitalWrite p $ v == w
-}