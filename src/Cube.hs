module Cube where
	
	import Prelude hiding (foldl)
	import Data.Monoid
	import Data.Functor
	import Data.Foldable
	import Data.Traversable
	import Control.Monad (replicateM)
	import qualified Data.EnumMap.Lazy as E
	import Util (allEnum)
	import Geometry

	data Pos = P1 | P2 | P3 deriving (Eq, Ord, Enum, Bounded, Show)
	type Pos3 = (Pos, Pos, Pos)

	allPos :: [Pos]
	allPos = allEnum

	allPos3 :: [Pos3]
	allPos3 = let t3 [x, y, z] = (x, y, z)
	          in t3 <$> replicateM 3 allEnum

	dirPos :: Dir -> Pos -> Maybe Pos
	dirPos Neutral  p  = Just p
	dirPos Negative P1 = Nothing
	dirPos Negative p  = Just $ pred p
	dirPos Positive P3 = Nothing
	dirPos Positive p  = Just $ succ p

	newtype Pos3M = Pos3M { pos3 :: Pos3 }

	instance Coords Pos3M where
		(Pos3M (px, py, pz)) ~> (dx, dy, dz) =
			do x <- dirPos dx px
			   y <- dirPos dy py
			   z <- dirPos dz pz
			   return $ Pos3M (x, y, z)

	type Dim2 a b c = a b (a b c)
	type Dim3 a b c = a b (Dim2 a b c)
	newtype Cube a = Cube (Dim3 E.EnumMap Pos a) deriving (Show)

	empty :: Cube a
	empty = Cube E.empty

	init :: a -> Cube a
	init n = let init' :: b -> E.EnumMap Pos b
	             init' m = E.fromList $ map (\p -> (p, m)) allEnum
	         in Cube $ init' $ init' $ init' $ n

	(!) :: Cube a -> Pos3 -> a
	(!) (Cube c) (x, y, z) = c E.! x E.! y E.! z

	_up :: (E.EnumMap Pos a -> E.EnumMap Pos a) 
	       -> (Pos, Pos) -> Cube a -> Cube a
	_up f (x, y) (Cube c) =
		let e = f (c E.! x E.! y)
		    d = E.insert y e (c E.! x)
		in Cube $ E.insert x d c

	insert :: Pos3 -> a -> Cube a -> Cube a
	insert (x, y, z) v = _up (E.insert z v) (x, y)

	adjust :: (a -> a) -> Pos3 -> Cube a -> Cube a
	adjust f (x, y, z) = _up (E.adjust f z) (x, y)

	instance Functor Cube where
		fmap f c =
			let get l = f $ c ! l
			    add s l = insert l (get l) s
			in foldl add empty allPos3

	instance Foldable Cube where
		foldMap f c =
			let get l = f $ c ! l
			    add s l = s <> get l
			in foldl add mempty allPos3

{-
	instance Traversable Cube where
		traverse f c = 
			let list = (c !) <$> allPos3
			in traverse f list\
-}

-- (a -> Maybe b) -> [a] -> Maybe [b]
-- cons_f :: a -> Maybe [b] -> Maybe [b]
-- (f x) :: Maybe b
-- ((:) <$> f x) == fmap (:) (f x) == 

-- traverse  :: (a -> Maybe b) -> Cube a -> Maybe (Cube b)
-- sequenceA :: Cube (Maybe a) -> Maybe (Cube a)
