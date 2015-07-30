module TicTacToe (
	Team,
	Issue,
	Board(..),
	Coord(..),
	Result,
	get,
	play,
	board
) where
	import Control.Monad
	import Data.Functor
	import Data.Monoid
	import Data.Maybe
	import Data.List
	import Data.Foldable as F
	import Data.Vector (Vector)
	import qualified Data.Vector as V
	import Data.Tuple.Homogenous
	import Util

	type Dim3 a b = a (a (a b))
	type V3 a = Dim3 Vector a

	-- | length of a side, and a board
	type Board = (Int, V3 Issue)

	-- | 3d (x/y/z) coordinates
	type Coord = (Int, Int, Int)

	-- | [1, 2, 3, 4, 5] -> [(1, 5), (2, 4)]
	collapse :: [a] -> [(a, a)]
	collapse ns = take (halfLen ns) (collapse' ns)
		where halfLen ms = length ms `quot` 2
		      collapse' ms = zip ms (reverse ms)

	-- | generates pairs of directions which are
	-- | facing against each other, in given dimension
	directions :: Int -> [([Int], [Int])]
	directions i = reverse $ collapse $ allDirections
		where allDirections = replicateM i [-1, 0, 1]

	-- | pretty much `directions 3`
	directions3 :: [Tuple2 Coord]
	directions3 = do
		d <- directions 3
		return $ do
			[x, y, z] <- Tuple2 d
			return (x, y, z)

	-- | generates a line to every direction
	explode :: Coord -> [Tuple2 [Coord]]
	explode c = (walk c <$>) <$> directions3
		where walk h i = let j = add h i in j : walk j i
		      add (h, i, j) (k, l, m) = (h + k, i + l, j + m)

	-- | gives lines of all directions
	-- | crossing given Coord within given length
	getLines :: Int -> Coord -> [[Coord]]
	getLines len crd = catMaybes $ do
		Tuple2 (fs, bs) <- explode crd
		let	line = crd : pick fs ++ pick bs where
				pick = takeWhile $ inside
				inside = F.all inside' . Tuple3
				inside' n = 0 <= n && n < len
		return $ (length line == len) `thenJust` line

	type Team = Bool
	type Issue = Maybe Team

	-- | B B B -> B
	-- | B B R -> D
	-- | B B - -> D
	foldI :: [Issue] -> Issue
	foldI []     = Nothing
	foldI (x:xs) = F.foldr add x xs
		where add m n = if m == n then m else Nothing
		-- not Monoid; mappend mempty x /= x

	-- | gets an content at the specified Coord
	get :: V3 a -> Coord -> a
	get v (x, y, z) = v V.! x V.! y V.! z

	set ::Coord -> a -> V3 a -> V3 a
	set (x, y, z) n = setf x $ setf y $ setv z n where
		setf i f y = setv i (f $ y V.! i) y
		setv i m y = y V.// [(i, m)]

	-- | Won team and line figurable from given coord and board
	check :: Board -> Coord -> Maybe ([Coord], Team)
	check (i, v) c = firstJust $ do
		l <- getLines i c
		let	i = foldI $ get v <$> l
		return $ (,) l <$> i

	type Result = Maybe (Either [Coord] Board)

	-- | puts one at the specified Coord and sees the issue
	-- | Just Left [Coord] → the player wins
	-- | Just Right Board   → the game goes on
	-- | Nothing            → got occupied Coord
	play :: Board -> Team -> Coord -> Result
	play (l, v) t c
		| get v c /= Nothing = Nothing
		| otherwise = 
			let new = (l, set c (Just t) v)
			in Just $ case check new c of
				Just (cs, _) -> Left  cs
				Nothing      -> Right new

	-- | initializes a board with given initial values
	board :: Int -> (Coord -> Issue) -> Board
	board i f = (,) i $
		V.generate i $ \x ->
			V.generate i $ \y ->
				V.generate i $ \z ->
					f (x, y, z)
{-
攻撃側、盤、座標を受け取り結果を返す
座標の指す地点の確認は内部で行う
引き分けの結果は考慮しない
空白でなければ Occupied
勝利すれば Coord
何も起きなければ Game
-}
{-
次元を変数にするなら必ずマクロを記述する必要がある
そこにきてお前はマクロなんて勉強する時間も気力もない
まず三次元に固定して課題を終わらせてから研究しろ

各次元の座標は穴なく連続している必要がある
その性質は静的に定義検証できないから
値を格納する構造はVectorに固定される
配列長の静的管理には努力しない

盤の座標は(0,0,0)から拡張すると定義する
ただしそれを静的に管理する必要はない
-}
{- Data.List#find is more convenient
	firstJust :: [Maybe a] -> Maybe a
	firstJust [] = Nothing
	firstJust (x @ (Just _):_) = x
	firstJust (Nothing:xs) = firstJust xs
-}
{-
指定の盤Bと座標Cにおいて観測できる勝敗Iを導く
Cに交差する線を順次Bに投射し有意な者を選択する
座標の有効性の判定にはOrdとtakeWhileを用いる
check :: Board -> Coord -> Issue
check (Board i b) c = firstJust $
	where
		included i (=, 0, <)= c && c < i
		sized i l = length l == i

		filter ((i ==) . length) $ takeWhile range <$> explode c
-}
{- 配列長の確認にMaybeは使用しない
	get :: V3 a -> Coord -> Maybe a
	get bx ((x, y, z)) = (bx V.!? x) >>= (V.!? y) >>= (V.!? z)
-}
{-
A game on a board ends with a line filled with one color
	where a board has 3 dimensions
		where each dimension has a common integral length
	where each line is defined by the start point and the direction
	where each point is either blue, red or empty
-}
{- Index is Int
	data Dir = Sit | For | Bac

	dir :: (Enum c) => Dir -> c -> c
	dir Sit = id
	dir For = succ
	dir Bac = pred
-}
{- 
	type Dir3 = (Dir, Dir, Dir)
	type Cor3 = (Cor, Cor, Cor)

	move3 :: Cor3 -> Dir3 -> Cor3
	move3 (x, y, z) (dx, dy, dz) =
		(move x dx, move y dy, move z dz)
-}
{- No need for static check of dimension
	data Nat = Zero | Succ Nat

	succ :: Nat -> Nat
	succ = Succ

	pred :: Nat -> Maybe Nat
	pred Zero = Nothing
	pred (Succ n) = Just n

	toInt :: Nat -> Int
	toInt Zero = 0
	toInt (Succ n) = 1 + toInt n

	fromInt :: Int -> Maybe Nat
	fromInt n | n <  0    = Nothing
	          | n == 0    = Just Zero
	          | otherwise = Succ <$> fromInt (n - 1)
-}