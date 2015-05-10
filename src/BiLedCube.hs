module BiLedCube where

	import Data.Functor
	import Control.Monad (replicateM, sequence_)
	import Data.EnumMap.Lazy ((!))
	import qualified Data.EnumMap.Lazy as E
	import qualified Data.List as L
	import Data.Tuple.Homogenous
	import System.Hardware.Arduino
	import Util (allEnum)
	import BiLed
	import Cube hiding ((!))

	type BiLedBoard = Dim2 E.EnumMap Pos BiLed
	type Grounds = E.EnumMap Pos Pin
	data BiLedCube = BiLedCube Grounds BiLedBoard
	type Pos2 = (Pos, Pos)

	allPos2 :: [Pos2]
	allPos2 = let t2 [x, y] = (x, y)
	          in t2 <$> replicateM 2 allEnum

	drawBoard :: BiLedCube -> Pos -> (Pos3 -> Tril) -> Arduino ()
	drawBoard (BiLedCube gs b) p t = do
		sequence_ $ do
			p' <- allPos
			return $ digitalWrite (gs ! p') (p' /= p)
		sequence_ $ do
			(y, z) <- allPos2
			return $ biLedWrite (b ! y ! z) $ t (p, y, z)

{-
	runBoard :: () -> LedBoard -> Arduino ()
	runBoard ol nl =
		let get' (x, y) l = l E.! x E.! y
		    get p = (get' p ol, get' p nl)
		    write (o, n) = biLedWrite n o
		in mapM_ write $ get <$> allPos2

	type Ground = E.EnumMap Pos Pin

	runGround :: Ground -> Arduino ()
	runGround c = mapM_ f allEnum where
		f :: Pos -> Arduino ()
		f p = mapM_ g (E.assocs c) where
			g :: (Pos, Pin) -> Arduino ()
			g (p', n) = digitalWrite n (p /= p')
-}

{-
	type BiLedCube = Cube BiLedOut
	type BiLedCubeGnd = E.EnumMap Pos Pin

	--biled :: Maybe Bool -> Location -> BiLedCube -> Arduino ()
	--biled t l c = biLedWrite (getW l c) t

	runGnds :: BiLedCubeGnd -> Arduino ()
	runGnds c = mapM_ f allEnum where
		f pos = mapM_ f' (E.assocs c) where
			f' (pos', pin) = digitalWrite pin (pos == pos')
-}
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