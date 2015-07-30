module Test where

	import Data.Monoid

	add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
	add a b = a <> b