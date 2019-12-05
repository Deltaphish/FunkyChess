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
moves (Piece King _)     = [(dr',dc') | dr' <- [-1..1], dc' <- [-1..1]]


onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r <= 7 && c >= 0 && c <= 7


-- Generates a list of possible destinations for the piece at startPos
possibleDest :: Board -> Pos -> [Pos]
possibleDest board startPos
   | (rank p) `elem` [Pawn,Knight,King] = step $ moves p
   | otherwise                          = concat [ walk startPos move | move <- moves p]
   where
      p = fromJust $ board #!> startPos

      step :: [Direction] -> [Pos]
      step = (filter onBoard).map (\(a,b) -> (a+(fst startPos),b+(snd startPos)))

      walk :: Pos -> Direction -> [Pos]
      walk (r,c) (dr,dc)
         | not $ onBoard newPos = []
         | otherwise = case board #!> newPos of
                          Nothing -> newPos : walk newPos (dr,dc)
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



{-
    -- various test functions, delete later
    vizDest :: Board -> Pos -> Board
    vizDest board pos = foldr (\p b -> b #=> (p,Just(Piece Queen White))) board $ possibleDest board pos

    testBoard = Board $ replicate 3 emptyRow ++
                (replicate 3 Nothing ++ Just (Piece Queen Black) : replicate 4 Nothing) :
                replicate 4 emptyRow
-}

-- Pure code ends here

                    


