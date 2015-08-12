{-|
	Defines a 3-layered Vector and relative utilties.
-}
module Game.TicTacToe3D.Vector3 where
	import Data.List (unfoldr)
	import Data.Vector (Vector)
	import qualified Data.Vector as V

	{-|
		Represents a 3-layered structure.
	-}
	type Dim3 a b = a (a (a b))

	{-|
		Represents a 3-layered vector.
	-}
	type V3 a = Dim3 Vector a

	{-|
		Represents an index of a 3-layered vector.
	-}
	type I3 = (Int, Int, Int)

	{-|
		>>> base 2 10
		[0, 1, 0, 1]
	-}
	base :: Int -> Int -> [Int]
	base i n = unfoldr f n ++ repeat 0 where
		f b = if b /= 0 then Just (pair b i) else Nothing where
			pair n i = (n `mod` i, n `div` i)

	{-|
		>>> i3 1
		(1, 0, 0)
		
		>>> i3 26
		(1, 1, 1)
	-}
	i3 :: Int -> I3
	i3 i = t $ base 3 i where t (x:y:z:_) = (x, y, z)

	{-|
		Retrieves the specified location's element.
	-}
	(!) :: V3 a -> I3 -> a
	v ! (x, y, z) = v V.! x V.! y V.! z

	{-|
		Replaces the specified location's element
		and retrieves the updated vector.
	-}
	(//) ::V3 a -> (I3, a) -> V3 a
	v // ((x, y, z), n) = (setf x $ setf y $ setv z n) v where
		setf i f y = setv i (f $ y V.! i) y
		setv i m y = y V.// [(i, m)]

	{-|
		Initializes a vector with the given values.
	-}
	init :: Int -> (I3 -> a) -> V3 a
	init i f =
		V.generate i $ \x ->
			V.generate i $ \y ->
				V.generate i $ \z ->
					f (x, y, z)
