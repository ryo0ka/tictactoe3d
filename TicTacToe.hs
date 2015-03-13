module TicTacToe where

	import Util
	import Direction
	import World
	import WorldDirected
	import Data.Tuple.Homogenous
	import Data.Maybe (fromMaybe)

	data Situation = Player1 | Player2 | None deriving (Eq, Show)
	type TicTacToe = World Situation

	toPlayer :: Bool -> Situation
	toPlayer True  = Player1
	toPlayer False = Player2

	against :: Situation -> Situation
	against Player1 = Player2
	against Player2 = Player1
	against None    = None

	addS :: Situation -> Situation -> Situation
	addS p1 p2 = if p1 == p2 then p1 else None

	foldS :: [Situation] -> Situation
	foldS ss = let
		addS' Nothing p   = Just p
		addS' (Just p) p' = Just $ addS p p'
		in fromMaybe None $ foldl addS' Nothing ss

	getS :: [Location] -> TicTacToe -> Situation
	getS ls@(_:_:_:[]) w = foldS $ map (flip getW w) ls
	getS _ _ = None

	check :: Location -> TicTacToe -> (Situation, Direction3)
	check l w = let
		dirs = filter (steady /=) directionsH
		nnone (s, _) = s /= None
		in headOr (None, steady) $ filter nnone $ do
			dir <- dirs
			let path = crossW l dir
			let sit = getS path w
			return (sit, dir)

	input :: Location 
		-> Situation 
		-> TicTacToe 
		-> Maybe (Situation, Direction3, TicTacToe)
	input l s w = if getW l w /= against s
		then let 
			w' = putW l s w
			(s', d) = check l w'
			in Just (s', d, w')
		else Nothing