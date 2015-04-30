module TicTacToe where

	import Prelude hiding (fold, foldr, foldl, init)
	import Data.Monoid
	import Data.Functor
	import Data.Foldable
	import Data.Traversable
	import Data.Tuple.Homogenous
	import Data.List (find)
	import Data.Maybe (isJust, catMaybes, fromMaybe)
	import qualified Data.Set as S
	import Control.Monad
	import Util (firstJust)
	import Geometry
	import Cube

	type Token = Bool
	type Sit = Maybe Token
	type T3Cube = Cube Sit

	newtype SitM = SitM { sit :: Sit }

	instance Monoid SitM where
		mempty = SitM Nothing
		mappend (SitM (Just s)) (SitM (Just t)) =
			SitM $ if s == t then Just s else Nothing
		mappend _ _ = SitM Nothing

	halfDirs :: [Dir3]
	halfDirs =
		let memberR d = S.member (reverse3 d)
		    insert' d m = if memberR d m then m else S.insert d m
		in S.toList $ foldr insert' S.empty allDirs

	line :: Pos3 -> Dir3 -> Maybe (Pos3, Pos3, Pos3)
	line p d = case (Pos3M p <~> d) of
		(x:y:z:[]) -> Just (pos3 x, pos3 y, pos3 z)
		_          -> Nothing

	checkLine :: T3Cube -> Pos3 -> Dir3 -> Sit
	checkLine c p d = do
		l <- line p d
		let ss = (c !) <$> Tuple3 l
		sit $ fold $ SitM <$> ss

	checkPos :: T3Cube -> Pos3 -> Maybe (Token, Dir3)
	checkPos c p = 
		firstJust $ do
			d <- halfDirs
			return $ do
				t <- checkLine c p d
				return (t, d)

{-
	firstJust $ do -- Maybe (Sit, Dir3)
		d <- halfDirs
		let ms = do
			l <- Tuple3 $ exact3 $ p <~> d
			return $ fold $ (c !) <$> l
		return $ do
			s <- ms
			return (s, d)
-}

{-
	check l w = let
		dirs = filter (steady /=) directionsH
		nnone (s, _) = s /= None
		in headOr (None, steady) $ filter nnone $ do
			dir <- dirs
			let path = crossW l dir
			let sit = getC path w
			return (sit, dir)

	getC :: [Location] -> CubeT3 -> Token
	getC ls@(_:_:_:[]) w = foldC $ map (flip getW w) ls
	getC _ _ = None

	input :: Location 
		-> Token 
		-> CubeT3 
		-> Maybe (Token, Dir3, CubeT3)
	input l s w = if getW l w /= oppose s
		then let 
			w' = putW l s w
			(s', d) = check l w'
			in Just (s', d, w')
		else Nothing
-}
