{-- Remove comments if running on windows --}
{-- Otherwise unicode errors will occur   --}

import ChessAI
import Board
import Types
--import GHC.IO.Encoding
--import System.Win32.Console
import Data.Maybe
import Data.List

numbs = "87654321"
chars = "ABCDEFGH"

main = do 
    --setLocaleEncoding utf8
    --setConsoleOutputCP 65001
    putStrLn "\nWelcome to FunkyChess \n-------------------- \nCommands:\n\nEnter the location of a piece to see all possible destinations, Example:B2 \nEnter the location of a piece and the destination with a space between to make a move, Example:B2 B3 \n\nPress Enter to start"
    getLine
    let board = initBoard
    print board
    gameloop board White


gameloop :: Board -> Color -> IO ()
gameloop b c = do
    putStr "Please enter a command for player "
    print $ show c
    putStrLn ""
    i <- getLine
    let pos = parseInput i
    if pos /= [] then
        if length pos == 1 then
            if ownPiece b (pos !! 0) c then
                putStrLn $ unlines $ "" : (map moveDestStr $ possibleMoves b (pos !! 0)) 
            else 
                putStrLn "Invalid input!\n"
        else 
            if correctMove b (pos !! 0) (pos !! 1)&& ownPiece b (pos !! 0) c then
                if c == White then do
                    if (pos !! 1) `elem` getPiecePositions b Black then
                        if (Piece King Black) == fromJust (b #!> (pos !! 1)) then
                            finish White
                        else do
                            print $ show c
                            putStr " Took a "
                            print $ rank $ fromJust (b #!> (pos !! 1))
                            let newb = fromJust $ movePiece b ((pos !! 0), (pos !! 1))
                            print newb
                            gameloop newb Black
                    else do
                        let newb = fromJust $ movePiece b ((pos !! 0), (pos !! 1))
                        print newb
                        gameloop newb Black
                else do
                    if (pos !! 1) `elem` getPiecePositions b White then
                        if (Piece King White) == fromJust (b #!> (pos !! 1)) then
                            finish Black
                        else do
                            print $ show c
                            putStr " Took a "
                            print $ rank $ fromJust (b #!> (pos !! 1))
                            let newb = fromJust $ movePiece b ((pos !! 0), (pos !! 1))
                            print newb
                            gameloop newb White
                    else do
                        let newb = fromJust $ movePiece b ((pos !! 0), (pos !! 1))
                        print newb
                        gameloop newb White
            else 
                putStrLn "Invalid input!\n"
    else 
        putStrLn "Invalid input!\n"
    gameloop b c


ownPiece :: Board -> Pos -> Color -> Bool
ownPiece b p c = p `elem` getPiecePositions b c

correctMove :: Board -> Pos -> Pos -> Bool
correctMove b p d = (p, d) `elem` possibleMoves b p

parseInput :: [Char] -> [Pos]
parseInput i
    | pieceInput i = [((fromJust $ elemIndex (i !! 1) numbs),(fromJust $ elemIndex (i !! 0) chars))]
    | moveInput i  = [((fromJust $ elemIndex (i !! 1) numbs),(fromJust $ elemIndex (i !! 0) chars)), ((fromJust $ elemIndex (i !! 4) numbs),(fromJust $ elemIndex (i !! 3) chars))]
    | otherwise    = []

pieceInput :: [Char] -> Bool
pieceInput i = 
    (length i == 2) && (((i !! 0) `elem` chars) && ((i !! 1) `elem` numbs))

moveInput :: [Char] -> Bool
moveInput i = 
    (length i == 5) && (i !! 0) `elem` chars && (i !! 1) `elem` numbs && (i !! 3) `elem` chars && (i !! 4) `elem` numbs 

moveDestStr :: Move -> [Char]
moveDestStr m = [chars !! (snd (snd m))] ++ [numbs !! (fst (snd m))]

finish :: Color -> IO ()
finish c = do
    putStr "Game is over!, Winner is "
    print $ show c
    putStrLn "\nPress Enter to play again..."
    getLine
    let board = initBoard
    print board
    gameloop board White