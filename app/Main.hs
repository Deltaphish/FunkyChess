{-- Remove comments if running on windows --}
{-- Otherwise unicode errors will occur   --}

import ChessAI
import Board
import Types
import GHC.IO.Encoding
--import System.Win32.Console
import Data.Maybe
import Data.List

numbs = "87654321"
chars = "ABCDEFGH"

main = do 
  --  setLocaleEncoding utf8
    --setConsoleOutputCP 65001
    putStrLn "\nWelcome to FunkyChess \n-------------------- \nCommands:\n\nEnter the location of a piece to see all possible destinations, Example:B2 \nEnter the location of a piece and the destination with a space between to make a move, Example:B2 B3 \n\nPress Enter to start"
    getLine
    startGame White


gameloop :: Board -> Color -> IO ()
gameloop b c = do
    putStr "Please enter a command for player "
    putStrLn $ show c
    i <- getLine
    handleInput b c $ parseInput i

handleInput :: Board -> Color -> [Pos] -> IO ()
handleInput  b c pos
    | length pos == 1 = getPosDest b c (head pos)
    | length pos == 2 = handleMove b c (makeMove b c ((head pos), (pos !! 1)))
    | otherwise       = do
        putStrLn "Invalid input!\n"
        gameloop b c

handleMove :: Board -> Color -> InputResult -> IO ()
handleMove b c (InvalidMove)     = do 
    putStrLn "Invalid input!\n"
    gameloop b c
handleMove b c (ValidMove f b')
    | f == Check c     = do
        putStr "Check for "
        putStrLn $ show c
        print b'
        gameloop b' (opponent c)
    | f == Checkmate c = do
        putStr "Checkmate for "
        putStrLn $ show c
        print b'
        finish c
    | otherwise        = do
        print b'
        gameloop b' (opponent c)

getPosDest :: Board -> Color -> Pos -> IO ()
getPosDest b c p
    | ownPiece b p c   = do 
        putStrLn $ unlines $ "" : map moveDestStr (possibleMoves b p) 
        gameloop b c
    | otherwise        = do 
        putStrLn "Invalid input!\n"
        gameloop b c

ownPiece :: Board -> Pos -> Color -> Bool
ownPiece b p c = p `elem` getPiecePositions b c

parseInput :: String -> [Pos]
parseInput i
    | pieceInput i = [(fromJust $ elemIndex (i !! 1) numbs,fromJust $ elemIndex (head i) chars)]
    | moveInput i  = [(fromJust $ elemIndex (i !! 1) numbs,fromJust $ elemIndex (head i) chars), (fromJust $ elemIndex (i !! 4) numbs, fromJust $ elemIndex (i !! 3) chars)]
    | otherwise    = []

pieceInput :: String -> Bool
pieceInput i = 
    (length i == 2) && ((head i `elem` chars) && ((i !! 1) `elem` numbs))

moveInput :: String -> Bool
moveInput i = 
    (length i == 5) && head i `elem` chars && (i !! 1) `elem` numbs && (i !! 3) `elem` chars && (i !! 4) `elem` numbs 

moveDestStr :: Move -> String
moveDestStr m = (chars !! snd (snd m)) : [numbs !! fst (snd m)]

startGame :: Color -> IO ()
startGame c = do
    let board = initBoard
    print board
    gameloop board c

finish :: Color -> IO ()
finish c = do
    putStr "Game is over!, Winner is "
    print $ show c
    putStrLn "\nPress Enter to play again..."
    getLine
    startGame White