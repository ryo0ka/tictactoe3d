
import Util
import Direction
import World
import WorldDirected
import TicTacToe
import Data.Tuple.Homogenous
import qualified Data.EnumMap.Lazy as E
import Data.Maybe (fromJust)

tab' Player1 = "O"
tab' Player2 = "X"
tab' None    = " "
tab ps = concat $ map tab' ps

loc :: Int -> Int -> Int -> Location
loc x y z = fromJust $ fromCoords $ Tuple3 (x, y, z)

printTTT :: TicTacToe -> IO ()
printTTT w = mapM_ f (E.elems w) where
	f w' = mapM_ f' (E.elems w') >> print "" where
		f' w'' = print $ tab $ E.elems w''

turn :: Bool -> TicTacToe -> IO (Situation, TicTacToe)
turn p w = do
	inp <- getLine
	let c = read inp :: (Int, Int, Int)
	let goback = turn p w
	case fromCoords (Tuple3 c) of
		Nothing -> print "Out of bounds" >> goback
		Just l  -> case input l (toPlayer p) w of
			Nothing -> print "Already taken" >> goback
			Just (s, _, w') -> return (s, w')

game :: Bool -> TicTacToe -> IO ()
game p w = do
	r <- turn p w
	case r of
		(None, w') -> printTTT w' >> game (not p) w'
		(p'  , w') -> print $ show p' ++ " won!"

main :: IO ()
main = game True $ initW None