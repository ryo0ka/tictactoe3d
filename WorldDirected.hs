module WorldDirected where

	import Util
	import World
	import Direction
	import qualified Data.Traversable as T

	toCoords :: Location -> Coords
	toCoords = fmap fromEnum

	fromCoords :: Coords -> Maybe Location
	fromCoords c = T.sequence $ fmap toEnumS c

	crossW :: Location -> Direction3 -> [Location]
	crossW l d = let
		c = toCoords l
		f c' d' = catMaybes' $ map fromCoords $ directL c' d'
		in l : merge (f c d) (f c (oppose3 d))