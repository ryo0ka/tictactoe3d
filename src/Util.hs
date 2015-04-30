module Util where

	import Data.Maybe
	import Data.List (find)

	sift :: (a -> Bool) -> a -> Maybe a
	sift f a = if f a then Just a else Nothing

	merge :: [a] -> [a] -> [a]
	merge xs     []     = xs
	merge []     ys     = ys
	merge (x:xs) (y:ys) = x : y : merge xs ys

	headMaybe :: [a] -> Maybe a
	headMaybe [] = Nothing
	headMaybe (x:_) = Just x

	headOr :: a -> [a] -> a
	headOr def list = fromMaybe def $ listToMaybe list

	catMaybes2 :: [Maybe a] -> [a]
	catMaybes2 ms = map fromJust $ takeWhile isJust ms

	_toEnumS :: (Enum a) => a -> a -> Int -> Maybe a
	_toEnumS min max i = let
		min' = fromEnum min
		max' = fromEnum max
		in if min' <= i && i <= max'
			then Just (toEnum i) 
			else Nothing

	toEnumS :: (Enum a, Bounded a) => Int -> Maybe a
	toEnumS = _toEnumS minBound maxBound

	allEnum :: (Enum a, Bounded a) => [a]
	allEnum = [minBound ..]

	firstJust :: [Maybe a] -> Maybe a
	firstJust xs = fromJust $ find isJust xs