module Util where

	import Data.Maybe

	sift :: (a -> Bool) -> a -> Maybe a
	sift f a = if f a then Just a else Nothing

	toEnumS' :: (Enum a) => a -> a -> Int -> Maybe a
	toEnumS' min max i = let
		min' = fromEnum min
		max' = fromEnum max
		in if min' <= i && i <= max'
			then Just (toEnum i) 
			else Nothing

	toEnumS :: (Enum a, Bounded a) => Int -> Maybe a
	toEnumS = toEnumS' minBound maxBound

	allEnum :: (Enum a, Bounded a) => [a]
	allEnum = [minBound ..]

	merge :: [a] -> [a] -> [a]
	merge xs     []     = xs
	merge []     ys     = ys
	merge (x:xs) (y:ys) = x : y : merge xs ys

	headOr :: a -> [a] -> a
	headOr def list = fromMaybe def $ listToMaybe list

	catMaybes' :: [Maybe a] -> [a]
	catMaybes' ms = map fromJust $ takeWhile isJust ms