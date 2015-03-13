module World where
	-- Utilities to handle 3*3*3 coorded World.

	import Util
	import Data.Tuple.Homogenous
	import qualified Data.EnumMap.Lazy as E

	data Position = P1 | P2 | P3 deriving (Eq, Ord, Enum, Bounded, Show)
	type Location = Tuple3 Position
	type Dim2 a b c = a b (a b c)
	type Dim3 a b c = a b (Dim2 a b c)
	type World a = Dim3 E.EnumMap Position a

	initW :: a -> World a
	initW n = let
		init' :: b -> E.EnumMap Position b
		init' m = E.fromList $ map (\p -> (p, m)) allEnum
		in init' $ init' $ init' $ n

	listW :: [(Location, a)] -> World a
	listW = foldl (\w (l, n) -> putW l n w) E.empty

	getW :: Location -> World a -> a
	getW (Tuple3 (x, y, z)) w = w E.! x E.! y E.! z

	putW :: Location -> a -> World a -> World a
	putW (Tuple3 (x, y, z)) n = let
		put = E.insert
		put' k v m = put k (v (m E.! k)) m
		in put' x $ put' y $ put z n
