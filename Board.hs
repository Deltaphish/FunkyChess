module Board ( Color (White,Black)
             , Rank (Pawn, Tower, Knight, Bishop, Queen, King)
             , Piece (Piece)
             , Board
             , (#!>)
             , (#=>)
             , Pos
             , Move
             , initBoard
             , possibleMoves
             , possibleDest
             , movePiece
             , getPiecePositions
             ) where

import Data.List
import Data.Maybe

type Pos = (Int,Int)
type Direction = (Int,Int)
type Move = (Pos,Pos)

data Color = White | Black deriving Eq

data Rank =
    Pawn   |
    Tower  |
    Knight |
    Bishop |
    Queen  |
    King deriving Eq

data Piece = Piece{ rank :: Rank
                  , color :: Color
                  }

type Square = Maybe Piece

newtype Board = Board [[Square]]

onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r <= 7 && c >= 0 && c <= 7

instance Show Piece
   where
    show (Piece Pawn White) = "♙"
    show (Piece Knight White) = "♘"
    show (Piece Bishop White) = "♗"
    show (Piece Tower White) = "♖"
    show (Piece Queen White) = "♕"
    show (Piece King White) = "♔"

    show (Piece Pawn Black) = "♟"
    show (Piece Knight Black) = "♞"
    show (Piece Bishop Black) = "♝"
    show (Piece Tower Black) = "♜"
    show (Piece Queen Black) = "♛"
    show (Piece King Black) = "♚"

instance Show Board where
   show (Board matrix) =  createGrid (map showRow matrix) ++ "  A B C D E F G H\n"
      where 
         createGrid = unlines.addNumberCol.addFloor

         addFloor :: [String] -> [String]
         addFloor = intersperse (init (concat (replicate 8 "— ")) ++ "|")

         addNumberCol :: [String] -> [String]
         addNumberCol = map (\(a,b) -> a ++ "|" ++ b) . zip (intersperse " " $ map show $ reverse [1..8])

         showRow [] = ""
         showRow (Just p:row') = show p ++ "|" ++ showRow row'
         showRow (Nothing:row') = " |" ++ showRow row'

{--- Helper functions for operating on a board ---}
-- Get square from Board
(#!>) :: Board -> Pos -> Square
(Board b) #!> (r,c) = b !! r !! c

-- Place a square on a position on a Board
(#=>) :: Board -> (Pos,Square) -> Board
(Board b) #=> ((r,c),sqr) = Board $ startRow ++ (startCol ++ sqr : endCol) : tail endRow
    where
        (startRow,endRow) = splitAt r b
        (startCol,_:endCol) = splitAt c (head endRow)


{--- Functions for creating the board ---}

emptyRow :: [Square]
emptyRow = replicate 8 Nothing

homeRow :: Color -> [Square]
homeRow color' = map Just homeRow'
    where homeRow' = [Piece Tower color',
                      Piece Knight color',
                      Piece Bishop color',
                      Piece Queen color',
                      Piece King color',
                      Piece Bishop color',
                      Piece Knight color',
                      Piece Tower color']

pawnRow :: Color -> [Square]
pawnRow color' = replicate 8 $ Just (Piece Pawn color')

initBoard :: Board
initBoard = Board $ 
               [homeRow Black,pawnRow Black] ++
               replicate 4 emptyRow          ++
               [pawnRow White,homeRow White]


{--- Functions for generating legal possibleDirections ---}

-- Returns a list of possible direction for the piece
-- This does not account for pieces with requirements (Pawn)

possibleDirections :: Piece -> [Direction]
possibleDirections (Piece Pawn White) = [(-1,0)]
possibleDirections (Piece Pawn Black) = [(1,0)]
possibleDirections (Piece Tower _ )   = [(1,0),(-1,0),(0,1),(0,-1)]
possibleDirections (Piece Knight _ )  = [(-2,1),(-2,-1),(-1,2),(-1,-2),
                           (2,1),(2,-1),(1,2),(1,-2)]
possibleDirections (Piece Bishop _ )  = [(1,1),(1,-1),(-1,1),(-1,-1)]
possibleDirections (Piece Queen _)    = possibleDirections (Piece Tower White) ++ possibleDirections (Piece Bishop White)
possibleDirections (Piece King _)     = [(dr',dc') | dr' <- [-1..1], dc' <- [-1..1], (dr',dc') /= (0,0)]


getPiecePositions :: Board -> Color -> [Pos]
getPiecePositions (Board b) color' = getPieceInRows b 0
   where
      getPieceInRows :: [[Square]] -> Int -> [Pos]
      getPieceInRows [] _     = []
      getPieceInRows (r:rw) n = [(n,c) | c <- findIndices isJust r, color (fromJust (Board b #!> (n,c))) == color'] ++
                                getPieceInRows rw (n+1)

-- Generates a list of possible destinations for the piece at startPos
-- set checkCoverage to true for checking if a piece is covering another friendly piece
possibleDest :: Board -> Bool -> Pos -> [Pos]
possibleDest board checkCoverage startPos
   | rank p == Pawn                   = pawnWalk
   | rank p `elem` [Knight,King]      = concat [ walk startPos dir False | dir <- possibleDirections p]
   | otherwise                        = concat [ walk startPos dir True  | dir <- possibleDirections p]
   where
      p = fromMaybe (error ("possibleDest: no piece at position " ++ show startPos))
             (board #!> startPos)

      -- Special case for pawn since pawn can only go diagonal if it can take a piece and two steps if first step
      pawnWalk :: [Pos]
      pawnWalk = canForward ++ canTwoStep ++ concatMap canDiagonal [-1,1]
         where
            (dr,_) = head $ possibleDirections p
            (r,c)  = startPos
            canTwoStep :: [Pos]
            canTwoStep | isOnHomeRow = walk startPos (dr*2,0) False
                       | otherwise   = []
            canDiagonal :: Int -> [Pos]
            canDiagonal c' | isValidTarget (r',c'+c) = walk startPos (dr,c') False
                           | otherwise               = []
                              where r' = r+dr
            canForward  :: [Pos]
            canForward = walk startPos (dr,0) False
            isOnHomeRow :: Bool
            isOnHomeRow | color p == White = fst startPos == 6
                        | otherwise        = fst startPos == 1
            isValidTarget :: Pos -> Bool
            isValidTarget targetPos | onBoard targetPos = case board #!> targetPos of
                                                               Nothing -> False
                                                               Just p' -> checkCoverage || color p /= color p'
                                    | otherwise         = False
      
      walk :: Pos -> Direction -> Bool -> [Pos]
      walk (r,c) (dr,dc) multiStep
         | not $ onBoard newPos = []
         | otherwise = case board #!> newPos of
                          Nothing -> if multiStep then newPos : walk newPos (dr,dc) multiStep else [newPos]
                          Just p' -> if checkCoverage || color p' == color p
                                        then []
                                        else [newPos]
            where newPos = (r+dr,c+dc)


possibleMoves :: Board -> Pos -> [Move]
possibleMoves board p = zip (repeat p) (possibleDest board False p)

movePiece :: Board -> Move -> Maybe Board
movePiece board (start,dest)
   | dest `elem` possibleDest board False start = Just $ (board #=> (dest,board #!> start)) #=> (start,Nothing)
   | otherwise                                  = error "Invalid Move"