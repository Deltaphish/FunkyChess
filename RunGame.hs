module RunGame where
import Data.Char
import Data.Maybe
import System.Random
import Pieces

data Interface = Interface
  { iInitBoard    :: Board
  , iMoves        :: Piece -> [Direction]
  , iOnBoard      :: Pos -> Bool
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
  where x = digitToInt (s !! 0)
        y = digitToInt (s !! 1)        