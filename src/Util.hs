module Util where
	import Control.Monad
	import Control.Monad.State as S
	import Control.Applicative
	import Data.Functor
	import Data.Maybe
	import Data.List
	import Data.Traversable

	-- | if given bool is true, wraps another arg with Just, otherwise Nothing
	thenJust :: Bool -> a -> Maybe a
	thenJust b n = if b then Just n else Nothing

	-- | retrieves the first found Just in the list, or Nothing if not found any
	firstJust :: [Maybe a] -> Maybe a
	firstJust ms = join $ find isJust ms

	-- | base 2 10 == [0, 1, 0, 1]
	base :: Int -> Int -> [Int]
	base i n = unfoldr f n ++ repeat 0
		where f b = (b /= 0) `thenJust` (b `mod` i, b `div` i)

	-- | [A, B, C] -> [(0, A), (1, B), (2, C)]
	indexed :: (Integral n, Traversable t) => t a -> t (n, a)
	indexed t = evalState (traverse go t) 0 where
		go a = flip (,) a <$> S.get <* modify succ

	allEnum :: (Bounded a, Enum a) => [a]
	allEnum = [minBound..maxBound]

	