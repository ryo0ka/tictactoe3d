module BiLedCube (
	Cube(..),
	Lyr(..),
	succLayer,
	drawLayer,
	drawCube,
) where
	import Control.Monad
	import Data.Functor
	import Data.Traversable
	import Data.Foldable as F
	import Data.List
	import Data.Tuple.Homogenous
	import System.Hardware.Arduino
	import BiLed
	import Util

	type Grounds = Tuple3 Pin
	type Layer = Tuple9 BiLed
	data Cube = Cube Grounds Layer

	drawLayer :: Layer -> ((Int, Int) -> Maybe Bool) -> Arduino ()
	drawLayer ls f = F.sequence_ $ do
		(yz, l) <- indexed ls
		let	(y:z:_) = succ <$> base 2 yz
		return $ biLedWrite l $ f (y, z)

	data Lyr = LA | LB | LC deriving (Eq, Enum, Bounded)

	succLayer :: Lyr -> Lyr
	succLayer LC = LA
	succLayer l = succ l

	parGnds :: Grounds -> Lyr -> ((Int, Pin), [(Int, Pin)])
	parGnds gnds lyr = 
		headFst $ partition match $ indexed $ toList gnds where
			headFst ([x], xs) = (x, xs)
			match (i, _) = fromEnum lyr == i

	drawCube :: Cube -> Lyr -> ((Int, Int, Int) -> Maybe Bool) -> Arduino ()
	drawCube (Cube gs ls) i f = do
		let ((x, g), rgs) = parGnds gs i
		F.sequence_ $ do
			(_, rg) <- rgs
			return $ digitalWrite rg True
		digitalWrite g False
		drawLayer ls $ (\(y, z) -> f (x, y, z))
{-
一列目から三列目まで走査するうち
現時点の列の凹を消灯し凸を走査し凹を点灯する
開始前後を考えから除けば自然
-}
{-
凸型出力を値に持つ二層の線型構造体と凹型出力の組を線型構造体に収める
全ての線型構造体にVectorを用いる
全ての線型構造体の辺長は3とする
-}
