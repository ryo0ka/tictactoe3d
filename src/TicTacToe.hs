-- Defines a state machine of 3D Tic-Tac-Toe.
module TicTacToe (
	Team,
	Issue,
	Board,
	Game (..),
	done,
	newGame,
	playGame,
	explode'
) where
	import Control.Monad
	import Data.Functor
	import Data.Monoid
	import Data.Maybe
	import Data.List
	import Data.Foldable as F
	import Data.Tuple.Homogenous
	import Vector3 as V
	import Util (base, thenJust, firstJust)

	-- [1, 2, 3, 4, 5] -> [(1, 5), (2, 4)]
	collapse :: [a] -> [(a, a)]
	collapse ns = take (halfLen ns) (collapse' ns)
		where halfLen ms = length ms `quot` 2
		      collapse' ms = zip ms (reverse ms)

	-- In the specified dimension,
	-- generates all the possible directions,
	-- and pairs up every two of them that face to each other.
	directions :: Int -> [([Int], [Int])]
	directions i = collapse $ allDirections
		where allDirections = replicateM i [-1, 0, 1]

	-- Simplifies `directions 3`.
	directions3 :: [Tuple2 I3]
	directions3 = f <$> directions 3 where
		f t = g <$> Tuple2 t where
			g [x, y, z] = (x, y, z)

	-- Retrieves a line to every direction from the given point.
	explode :: I3 -> [Tuple2 [I3]]
	explode c = (walk c <$>) <$> directions3 where
		walk h i = let j = add h i in j : walk j i where
			add (h, i, j) (k, l, m) = (h + k, i + l, j + m)

	-- Checks if the given point is inside of
	-- the specified area in every three dimension.
	withinC :: Int -> Int -> I3 -> Bool
	withinC min max c = 
		F.all f $ Tuple3 c where
			f n = min <= n && n < max

	-- Retrieves all the possible lines that
	-- intersect at the given point. All those
	-- lines are within the area
	-- from 0 to the specified number.
	explode' :: Int -> I3 -> [[I3]]
	explode' len crd = catMaybes $ do
		Tuple2 (fs, bs) <- explode crd
		let	line = crd : pick fs ++ pick bs where
				pick = takeWhile $ withinC 0 len
		return $ (length line == len) `thenJust` line

	-- Represents a team.
	type Team = Bool

	-- Represents a state of one point in a board;
	-- owned by either team or empty.
	type Issue = Maybe Team

	-- Represents a tic-tac-toe board with its side length
	type Board = (Int, V3 Issue)

	-- B B B -> B
	-- B B R -> D
	-- B B - -> D
	foldI :: [Issue] -> Issue
	foldI []     = Nothing
	foldI (x:xs) = F.foldr add x xs
		where add m n = if m == n then m else Nothing
		-- not Monoid; mappend mempty x /= x

	-- Retrieves the winner and the owned line.
	-- Nothing if the game has not ended yet.
	check :: Board -> I3 -> Maybe ([I3], Team)
	check (i, v) c = firstJust $ do
		l <- explode' i c
		let	j = foldI $ (v V.!) <$> l
		return $ (,) l <$> j

	-- Represents a result of one team's action.
	type Result = Maybe (Either [I3] Board)

	-- Makes the given team play the specified square.
	-- Just Left [I3]   → The team has won.
	-- Just Right Board → The game goes on.
	-- Nothing          → The specified square is not playable.
	play :: Board -> Team -> I3 -> Result
	play (l, v) t c
		| v V.! c /= Nothing = Nothing
		| otherwise = 
			let new = (l, v V.// (c, Just t))
			in Just $ case check new c of
				Just (cs, _) -> Left  cs
				Nothing      -> Right new

	-- Initializes a board with the given values.
	initBoard :: Int -> (I3 -> Issue) -> Board
	initBoard i f = (,) i $ V.init i f

	-- Represents a state of a tic-tac-toe game.
	data Game = Game Board Team | Done Team [I3]

	-- Retrieves True if the given game has finished, otherwise False.
	done :: Game -> Bool
	done (Done _ _) = True
	done _ = False

	-- An initialized state of a game.
	newGame :: Game
	newGame = Game newBoard True where
		newBoard = (initBoard 3 $ const Nothing)

	-- Makes the current team play at the specified square.
	playGame :: Int -> Game -> Game
	playGame _ g @ (Done _ _) = g
	playGame c g @ (Game b t) =
		case play b t (i3 c) of
			Just (Left cs)  -> Done t cs
			Just (Right b') -> Game b' (not t)
			Nothing         -> g
