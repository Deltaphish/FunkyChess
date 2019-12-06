import Data.List
import Data.Maybe

type Pos = (Int,Int)
type Direction = (Int,Int)

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

data Board = Board [[Square]]

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

{-
instance Show Board
 where
    show (Board matrix) = (unlines $ map (\(a,b) -> a ++ "|" ++ b) $ zip (intersperse " " (map show (reverse [1..8]))) $intersperse (concat (replicate 8 "_ ")) $ map showRow matrix) ++ "  A B C D E F G H"
       where
        showRow [] = ""
        showRow ((Just p):row') = show p ++ "|" ++ showRow row'
        showRow ((Nothing):row') = " |" ++ showRow row'
-}
instance Show Board where
   show (Board matrix) =  createGrid (map showRow matrix) ++ "  A B C D E F G H"
      where 
         createGrid = unlines.addNumberCol.addFloor

         addFloor :: [String] -> [String]
         addFloor = intersperse (init (concat (replicate 8 "— ")) ++ "|")

         addNumberCol :: [String] -> [String]
         addNumberCol = map (\(a,b) -> a ++ "|" ++ b).(zip (intersperse " " $ map show $ reverse [1..8]))

         showRow [] = ""
         showRow ((Just p):row') = show p ++ "|" ++ showRow row'
         showRow ((Nothing):row') = " |" ++ showRow row'

-- Get from Board
(#!>) :: Board -> Pos -> Square
(Board b) #!> (r,c) = b !! r !! c
-- Place on Board
(#=>) :: Board -> (Pos,Square) -> Board
(Board b) #=> ((r,c),sqr) = Board $ startRow ++ (startCol ++ sqr : endCol) : (tail endRow)
    where
        (startRow,endRow) = splitAt r b
        (startCol,_:endCol) = splitAt c (head endRow)


emptyRow :: [Square]
emptyRow = replicate 8 Nothing

homeRow :: Color -> [Square]
homeRow color' = map Just homeRow'
    where 
       homeRow' = (Piece Tower color')  :
                  (Piece Knight color') :
                  (Piece Bishop color') :
                  (Piece Queen color')  :
                  (Piece King color')   :
                  (Piece Bishop color') :
                  (Piece Knight color') :
                  (Piece Tower color')  :
                  []

pawnRow :: Color -> [Square]
pawnRow color' = map Just $ replicate 8 (Piece Pawn color')

-- Returns a list of possible direction for the piece

moves :: Piece -> [Direction]
moves (Piece Pawn White) = (-1,0) : (-1,1) : (-1,-1) : []
moves (Piece Pawn Black) = (1,0) : (1,1) : (1,-1) : []
moves (Piece Tower _ )   = (1,0) : (-1,0) : (0,1) : (0,-1) : []
moves (Piece Knight _ )  = (-2,1) : (-2,-1) : (-1,2) : (-1,-2) : 
                           (2,1) : (2,-1) : (1,2) : (1,-2) : []
moves (Piece Bishop _ )  = (1,1) : (1,-1) : (-1,1) : (-1,-1) : []
moves (Piece Queen _)    = moves (Piece Tower White) ++ moves (Piece Bishop White) -- Color of no importance
moves (Piece King _)     = [(dr',dc') | dr' <- [-1..1], dc' <- [-1..1], (dr',dc') /= (0,0)]


onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r <= 7 && c >= 0 && c <= 7


-- Generates a list of possible destinations for the piece at startPos
possibleDest :: Board -> Pos -> [Pos]
possibleDest board startPos
   | (rank p) `elem` [Pawn,Knight,King] = concat [ walk startPos move False | move <- moves p]
   | otherwise                          = concat [ walk startPos move True  | move <- moves p]
   where
      p = fromJust $ board #!> startPos
      {-
      collision :: Board -> (Pos,Pos) -> Bool
      collision board (_,dest) = case destSqr of
                                    Just p2 -> color p /= color p2
                                    Nothing -> False
         where
            destSqr = board #!> dest

      step :: [Direction] -> [Pos]
      step = (filter (collision board)).(filter onBoard).map (\(a,b) -> (a+(fst startPos),b+(snd startPos))) -}

      walk :: Pos -> Direction -> Bool -> [Pos]
      walk (r,c) (dr,dc) b
         | not $ onBoard newPos = []
         | otherwise = case board #!> newPos of
                          Nothing -> if b then newPos : walk newPos (dr,dc) b else [newPos]
                          Just p' -> if color p' == color p
                                        then []
                                        else [newPos]
            where newPos = (r+dr,c+dc)

-- API functions
initBoard :: Board
initBoard = Board $ homeRow Black        :
            pawnRow Black                :
            replicate 4 emptyRow         ++
            pawnRow White                :
            homeRow White                :
            []


movePiece :: Board -> Pos -> Pos -> Maybe Board
movePiece board start dest
   | dest `elem` possibleDest board start = Just $ (board #=> (dest,board #!> start)) #=> (start,Nothing)
   | otherwise                            = Nothing

-- Returns the color of the player who has been checkmated
checkMate :: Board -> Maybe Color
checkMate = undefined
-- AI

pieceValue :: Piece -> Int
pieceValue (Piece r c) = rankValue r * colorWeight c
   where 
      colorWeight :: Color -> Int
      colorWeight White = -1
      colorWeight Black = 1

      rankValue :: Rank -> Int
      rankValue Pawn  = 3
      rankValue Queen = 50
      rankValue King  = 900
      rankValue _     = 25

valueBoard :: Board -> Int
valueBoard (Board b) = valueRow b 0
   where 
      valueRow :: [[Square]] -> Int -> Int
      valueRow [] _ = 0
      valueRow (r:rs) n = let rowVal = foldr (+) 0 $ map (\p -> pieceValue p) $ catMaybes r in rowVal + valueRow rs (n+1)

getPiecePositions :: Board -> [(Pos,Piece)]
getPiecePositions (Board b) = getPieceInRows b 0
   where
      getPieceInRows :: [[Square]] -> Int -> [(Pos,Piece)]
      getPieceInRows [] _     = []
      getPieceInRows (r:rw) n = [((n,c),fromJust (r !! c) )| c <- findIndices isJust r] ++
                                getPieceInRows rw (n+1)

minMax :: Board -> Color -> (Pos,Pos)
minMax board aiColor = getBestMove
   where
      aiPieces :: Board -> [(Pos,Piece)]
      aiPieces board = filter (\(_,p) -> color p == aiColor) $ getPiecePositions board
      
      possibleAiMoves :: [(Pos,Piece)] -> [(Pos,Pos)]
      possibleAiMoves = concat . map (\(cord,_) -> zip (repeat cord) (possibleDest board cord))

      possibleFutureBoards :: [(Pos,Pos)] -> [((Pos,Pos),Board)]
      possibleFutureBoards = map (\(s,d) -> ((s,d),fromJust $ movePiece board s d))

      scoreBoards :: [((Pos,Pos),Board)] -> [(Int,(Pos,Pos),Board)]
      scoreBoards = map (\(m,b) -> (valueBoard b,m,b))

      outcomes = scoreBoards.possibleFutureBoards.possibleAiMoves.aiPieces

      getBestMove = move where (_,move,_) = head $ sortBy (\(s1,_,_) (s2,_,_) -> compare s1 s2) $ outcomes board

aiMove :: Board -> Board
aiMove board = let (start,dest) = minMax board Black in fromJust $ movePiece board start dest
{-
    -- various test functions, delete later
    vizDest :: Board -> Pos -> Board
    vizDest board pos = foldr (\p b -> b #=> (p,Just(Piece Queen White))) board $ possibleDest board pos

    testBoard = Board $ replicate 3 emptyRow ++
                (replicate 3 Nothing ++ Just (Piece Queen Black) : replicate 4 Nothing) :
                replicate 4 emptyRow
-}

-- Pure code ends here

                    


