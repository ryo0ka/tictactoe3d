module Geometry where
 
 	import Prelude hiding (reverse)
	import Data.Monoid
	import Data.Functor
	import Control.Monad (replicateM)
	import Control.Applicative (liftA2)
	import Data.Tuple.Homogenous
	import Util (merge, allEnum)

	data Dir = Negative | Neutral | Positive
		deriving (Eq, Enum, Bounded, Ord, Show)

	reverse :: Dir -> Dir
	reverse Negative = Positive
	reverse Neutral  = Neutral
	reverse Positive = Negative

	type Dir3 = (Dir, Dir, Dir)

	neutral3 :: Dir3
	neutral3 = (Neutral, Neutral, Neutral)

	reverse3 :: Dir3 -> Dir3
	reverse3 d = untuple3 $ fmap reverse $ Tuple3 d

	allDirs :: [Dir3]
	allDirs = let t3 [x, y, z] = (x, y, z)
	      in t3 <$> replicateM 3 allEnum

	class Coords a where
		(~>) :: a -> Dir3 -> Maybe a

		-- initial coords is not included
		(~~>) :: a -> Dir3 -> [a]
		c ~~> d = case c ~> d of
			Just c' -> c' : c' ~~> d
			Nothing -> []

		-- initial coords is not included
		(<~>) :: a -> Dir3 -> [a]
		c <~> d = merge (c ~~> d) (c ~~> reverse3 d)

{-
	newtype Cor3 = Cor3 (Int, Int, Int) deriving (Ord, Eq, Show)

	instance Monoid Cor3 where
		mempty = Cor3 (0, 0, 0)
		mappend (Cor3 m) (Cor3 n) = 
			let tm = Tuple3 m
			    tn = Tuple3 n
			    r  = liftA2 (+) tm tn
			in Cor3 $ untuple3 $ r

	dirToCor :: Dir3 -> Cor3
	dirToCor (Dir3 d) = Cor3 $ untuple3 $ fmap fromEnum $ Tuple3 d

	instance Coords Cor3 where
		c ~> d = Just $ c <> dirToCor d
-}

{-
	class (Monoid a) => Coords a where
		fromDir :: Dir3 -> a

		(~>) :: a -> Dir3 -> a
		c ~> d = c <> fromDir d

		-- initial coords is not included
		(~~>) :: a -> Dir3 -> [a]
		c ~~> d = let c' = c ~> d in c' : c' ~~> d

		-- initial coords is not included
		(<~>) :: a -> Dir3 -> [a]
		c <~> d = merge (c ~~> d) (c ~~> reverse3 d)

	newtype Cor3 = Cor3 (Int, Int, Int) deriving (Ord, Eq, Show)

	instance Monoid Cor3 where
		mempty = Cor3 (0, 0, 0)
		mappend (Cor3 m) (Cor3 n) = 
			let tm = Tuple3 m
			    tn = Tuple3 n
			    r  = liftA2 (+) tm tn
			in Cor3 $ untuple3 $ r

	instance Coords Cor3 where
		fromDir (Dir3 d) = Cor3 $ untuple3 $ fmap fromEnum $ Tuple3 d
-}

{-
	instance Enum Dir where
		toEnum (-1) = Negative
		toEnum   0  = Neutral
		toEnum   1  = Coritive

		fromEnum Negative = -1
		fromEnum Neutral  =  0
		fromEnum Coritive =  1

	reverse :: Dir -> Dir
	reverse d = toEnum $ fromEnum d * (-1)
-}

{-
	(~>) :: Cor3 -> Dir3 -> Cor3
	c ~> d = c <> dirToCor d

	-- initial coords is not included
	(~~>) :: Cor3 -> Dir3 -> [Cor3]
	c ~~> d = let c' = c ~> d in c' : c' ~~> d

	-- initial coords is not included
	(<~>) :: Cor3 -> Dir3 -> [Cor3]
	c <~> dm = let dn = reverse3 dm
	           in merge (c ~~> dm) (c ~~> dn)
-}
