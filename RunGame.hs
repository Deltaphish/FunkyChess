module RunGame where
import Data.Char
import Data.Maybe
import Data.List
import System.Random
import Pieces

positions = "ABCDEFGH"

data Interface = Interface
  { iInitBoard    :: Board
  , iPossibleDest :: Board -> Pos -> [Pos]
  , iMovePiece    :: Board -> Pos -> Pos -> Maybe Board
  , iCheckMate    :: Board -> Maybe Color
  }

data Player = Guest | Ai
              deriving (Show, Eq)

runGame :: Interface -> IO ()
runGame i =
  do putStrLn "Welcome to FunkyChess"
     gameLoop i (iInitBoard i)

gameLoop :: Interface -> Board -> IO ()
gameLoop i b = do
  putStrLn $ show b
  putStrLn "Select piece"
  piecePos <- getLine
  putStrLn $ show (iPossibleDest i b (strToPos piecePos))
  putStrLn "Select destination from list"
  targetPos <- getLine
  gameLoop i $ fromJust (iMovePiece i b (strToPos piecePos) (strToPos targetPos))

strToPos :: String -> (Int,Int)
strToPos s = (x,y)
  where y = fromJust $ elemIndex (s !! 0) positions
        x = digitToInt (s !! 1) - 1       