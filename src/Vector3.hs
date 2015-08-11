-- Defines a 3-layered Vector and relative utilties.
module Vector3 where
	import Data.Vector (Vector)
	import qualified Data.Vector as V
	import Util (base)

	type Dim3 a b = a (a (a b))

	-- Represents a 3-layered vector.
	type V3 a = Dim3 Vector a

	-- Represents an index of a 3-layered vector.
	type I3 = (Int, Int, Int)

	-- i3  1 = (1, 0, 0)
	-- i3 26 = (1, 1, 1)
	i3 :: Int -> I3
	i3 i = t $ base 3 i where t (x:y:z:_) = (x, y, z)

	-- Retrieves the specified location's element.
	(!) :: V3 a -> I3 -> a
	v ! (x, y, z) = v V.! x V.! y V.! z

	-- Replaces the specified location's element
	-- and retrieves the updated vector.
	(//) ::V3 a -> (I3, a) -> V3 a
	v // ((x, y, z), n) = (setf x $ setf y $ setv z n) v where
		setf i f y = setv i (f $ y V.! i) y
		setv i m y = y V.// [(i, m)]

	-- Initializes with the given values.
	init :: Int -> (I3 -> a) -> V3 a
	init i f =
		V.generate i $ \x ->
			V.generate i $ \y ->
				V.generate i $ \z ->
					f (x, y, z)
