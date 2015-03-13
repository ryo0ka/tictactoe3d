module Direction where

	import Util (merge)
	import Data.Tuple.Homogenous
	import Data.Functor (fmap)
	import Control.Monad (replicateM)
	import Control.Applicative (liftA2)
	import qualified Data.Set as S

	data Direction = 
		Negative | Neutral | Positive
			deriving (Eq, Bounded, Ord, Show)

	instance Enum Direction where
		toEnum (-1) = Negative
		toEnum   0  = Neutral
		toEnum   1  = Positive

		fromEnum Negative = -1
		fromEnum Neutral  =  0
		fromEnum Positive =  1

	oppose :: Direction -> Direction
	oppose Neutral  = Neutral
	oppose Positive = Negative
	oppose Negative = Positive

	type Direction3 = Tuple3 Direction

	steady :: Direction3
	steady = Tuple3 (Neutral, Neutral, Neutral)

	oppose3 :: Direction3 -> Direction3
	oppose3 = fmap oppose

	directions :: [Direction3]
	directions = let
		comb  = [minBound .. maxBound]
		combs = replicateM 3 comb
		trans [x, y, z] = Tuple3 (x, y, z)
		in fmap trans combs

	directionsH :: [Direction3]
	directionsH = let
		put m d = if S.member (oppose3 d) m then m else S.insert d m
		in S.toList (foldl put S.empty directions)

	type Coords = Tuple3 Int

	addC :: Coords -> Coords -> Coords
	addC = liftA2 (+)

	settle :: Direction3 -> Coords
	settle = fmap fromEnum

	direct :: Coords -> Direction3 -> Coords
	direct c d = addC c (settle d)

	-- initial coords is not included
	-- steady direction is not cut off
	directL :: Coords -> Direction3 -> [Coords]
	directL c d = let c' = direct c d in c' : directL c' d

	-- steady direction is not cut off
	cross :: Coords -> Direction3 -> [Coords]
	cross c d = c : merge (directL c d) (directL c (oppose3 d))