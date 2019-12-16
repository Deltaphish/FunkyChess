{-- Remove comments if running on windows --}
{-- Otherwise unicode errors will occur   --}

import ChessAI
import Board
import Types
import GHC.IO.Encoding
import System.Win32.Console
import Data.Maybe
import Data.List

numbs = "87654321"
chars = "ABCDEFGH"
sides = [White, Black]

main = do 
    setLocaleEncoding utf8
    setConsoleOutputCP 65001
    putStrLn "\nWelcome to FunkyChess \n-------------------- \nCommands:\n\nEnter the location of a piece to see all possible destinations, Example:B2 \nEnter the location of a piece and the destination with a space between to make a move, Example:B2 B3 \n\nPress Enter to start"
    getLine
    startGame White


gameloop :: Board -> Color -> IO ()
gameloop b c = do
    putStr "Please enter a command for player "
    print $ show c
    putStrLn ""
    i <- getLine
    let pos = parseInput i
    if pos /= [] then
        if length pos == 1 then
            if ownPiece b (head pos) c then
                putStrLn $ unlines $ "" : map moveDestStr (possibleMoves b (head pos)) 
            else 
                putStrLn "Invalid input!\n"
        else 
            if correctMove b (head pos) (pos !! 1)&& ownPiece b (head pos) c then
                if c == White then do
                    putStr ""
                    handleInput b White pos
                else do
                    putStr ""
                    handleInput b Black pos
            else 
                putStrLn "Invalid input!\n"
    else 
        putStrLn "Invalid input!\n"
    gameloop b c
        where
            handleInput :: Board -> Color -> [Pos] -> IO ()
            handleInput b c pos =
                if (pos !! 1) `elem` getPiecePositions b (head (sides \\ [c])) then
                    if Piece King (head (sides \\ [c])) == fromJust (b #!> (pos !! 1)) then
                        finish c
                    else do
                        print $ show c
                        putStr " Took a "
                        print $ rank $ fromJust (b #!> (pos !! 1))
                        makeMove b c pos
                else
                    makeMove b c pos
                where
                    makeMove :: Board -> Color -> [Pos] -> IO ()
                    makeMove b c pos = do
                        let newb = fromJust $ movePiece b (head pos, pos !! 1)
                        print newb
                        gameloop newb (head (sides \\ [c]))


ownPiece :: Board -> Pos -> Color -> Bool
ownPiece b p c = p `elem` getPiecePositions b c

correctMove :: Board -> Pos -> Pos -> Bool
correctMove b p d = (p, d) `elem` possibleMoves b p

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