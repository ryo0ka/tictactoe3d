-- Defines general utilitiy functions
module Util where
	import Control.Monad
	import Control.Monad.State as S
	import Control.Applicative
	import Data.Functor
	import Data.Foldable as F
	import Data.Maybe
	import Data.List
	import Data.Traversable

	-- True  `thenJust` 1 = Just 1
	-- False `thenJust` 1 = Nothing
	thenJust :: Bool -> a -> Maybe a
	thenJust b n = if b then Just n else Nothing

	-- (True  `thenDo` succ) 1 = 2
	-- (False `thenDo` succ) 1 = 1
	thenDo :: Bool -> (a -> a) -> (a -> a)
	thenDo b f = if b then f else id

	-- firstJust [Nothing, Just 1 , Nothing] = Just 1
	-- firstJust [Nothing, Nothing, Nothing] = Nothing
	firstJust :: (Foldable f) => f (Maybe a) -> Maybe a
	firstJust ms = join $ F.find isJust ms

	-- base 2 10 = [0, 1, 0, 1]
	base :: Int -> Int -> [Int]
	base i n = unfoldr f n ++ repeat 0 where
		f b = (b /= 0) `thenJust` pair b i where
			pair n i = (n `mod` i, n `div` i)

	-- [A, B, C] -> [(0, A), (1, B), (2, C)]
	indexed :: Traversable t => t a -> t (Int, a)
	indexed t = evalState (traverse go t) 0 where
		go a = flip (,) a <$> S.get <* modify succ

	-- partitionI 2 [a, b, c, d] = Just (c, [a, b, d])
	-- partitionI 5 [a, b, c, d] = Nothing
	partitionI :: Int -> [a] -> Maybe (a, [a])
	partitionI i ns = format $ partition (i ==+) (indexed ns) where
		j ==+ k = j == fst k
		format ([], _) = Nothing
		format ([n], ns) = Just (snd n, snd <$> ns)
