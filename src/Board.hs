module Board ( (#!>)
             , (#=>)
             , initBoard
             , possibleMoves
             , possibleDest
             , movePiece
             , getPiecePositions
             , makeMove
             , prop_possibleDestOnBoard
             ) where

import Data.List
import Data.Maybe
import Types
import Test.QuickCheck

onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r <= 7 && c >= 0 && c <= 7



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

emptyBoard :: Board
emptyBoard = Board $ replicate 8 emptyRow

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
      pawnWalk | not $ onBoard (r+dr,c) = []  --- ToDo implement Pawn to Queen
               | otherwise = canForward ++ canTwoStep ++ concatMap canDiagonal [-1,1]
         where
            (dr,_) = head $ possibleDirections p
            (r,c)  = startPos
            canTwoStep :: [Pos]
            canTwoStep | isOnHomeRow && isNothing (collision (r+dr*2,c)) = walk startPos (dr*2,0) False
                       | otherwise   = []
            canDiagonal :: Int -> [Pos]
            canDiagonal c' | isValidTarget (r',c'+c) = walk startPos (dr,c') False
                           | otherwise               = []
                              where r' = r+dr
            canForward  :: [Pos]
            canForward | isNothing $ collision (r+dr,c) = walk startPos (dr,0) False
                       | otherwise                      = []

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

      collision :: Pos -> Maybe Color
      collision pos = color <$> board #!> pos


possibleMoves :: Board -> Pos -> [Move]
possibleMoves board p = zip (repeat p) (possibleDest board False p)


-- Make sure all destinations returned from possibleDest are on the board
prop_possibleDestOnBoard :: Piece -> Bool
prop_possibleDestOnBoard p = and $ map onBoard moves
   where
     positions = [(x,y) | x <- [0..7], y <- [0..7]]
     boards   = map (\pos -> (pos,emptyBoard #=> (pos,Just p))) positions
     moves   = concatMap (\(pos,board) -> possibleDest board False pos) boards



{-- Moving pieces --}

makeMove :: Board -> Color -> Move -> InputResult
makeMove board c (start,dest)
   | isNothing maybePiece                         = InvalidMove
   | color (fromJust maybePiece) /= c               = InvalidMove 
   | (start,dest) `notElem` possibleMoves board start  = InvalidMove
   | isJust flagColor && fromJust flagColor /= c      = InvalidMove
   | otherwise = ValidMove (getFlag board') board'
   where maybePiece = board #!> start 
         board' = movePiece board (start,dest)
         flag = getFlag board'
         flagColor = maybeFlagColor flag

getFlag :: Board -> Flag
getFlag board 
   | isCheckmate board White = Checkmate White
   | isCheckmate board Black = Checkmate Black
   | isCheck board White     = Check White 
   | isCheck board Black     = Check Black
   | otherwise              = Non

movePiece :: Board -> Move -> Board
movePiece board (start,dest)
   | dest `elem` possibleDest board False start = (board #=> (dest,board #!> start)) #=> (start,Nothing)
   | otherwise                                  = error "movePiece: Invalid Move"

{-- Checks for gamestate --}

isKing :: Board -> Pos -> Bool
isKing board p = case board #!> p of
                    Nothing -> False
                    Just p -> rank p == King 

kingPosition :: Board -> Color -> Maybe Pos
kingPosition board c = listToMaybe $ filter (isKing board) $ getPiecePositions board c

isCheck :: Board -> Color -> Bool
isCheck board c = elem kingPos $ concatMap (possibleDest board False) $ getPiecePositions board c
   where kingPos = fromJust $ kingPosition board (opponent c)

isCheckmate :: Board -> Color -> Bool
isCheckmate board c = isCheck board c && noEscape
   where 
      opponentMoves     = concatMap (possibleMoves board) $ getPiecePositions board (opponent c)
      allPossibleBoards = map (movePiece board) opponentMoves
      noEscape          = and $ map ((flip isCheck) c) allPossibleBoards


