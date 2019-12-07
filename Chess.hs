import Data.List
import Data.Maybe
import Control.Parallel.Strategies

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
moves (Piece Pawn White) = (-1,0) : []
moves (Piece Pawn Black) = (1,0) : []
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
   | (rank p) == Pawn                   = pawnWalk
   | (rank p) `elem` [Knight,King]      = concat [ walk startPos move False | move <- moves p]
   | otherwise                          = concat [ walk startPos move True  | move <- moves p]
   where
      p = fromJust $ board #!> startPos

      -- Special case for pawn since pawn can only go diagonal if it can take a piece
      pawnWalk = concat [pawnWalk' i | i <- [-1..1]]
         where
            (r,c)  = startPos
            (dr,_) = head $ moves p
            pawnWalk' :: Int -> [Pos]
            pawnWalk' 0
               | onBoard (r+dr,c) = (r+dr,c) : []
               | otherwise        = []
            pawnWalk' n
               | onBoard (r+dr,c+n) = case board #!> (r+dr,c+n) of
                                          Just piece -> if color piece /= color p then (r+dr,c+n) : [] else []
                                          Nothing -> []
               | otherwise = []
         

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


movePiece :: Board -> Move -> Maybe Board
movePiece board (start,dest)
   | dest `elem` possibleDest board start = Just $ (board #=> (dest,board #!> start)) #=> (start,Nothing)
   | otherwise                            = error "Invalid Move"

-- Returns the color of the player who has been checkmated
checkMate :: Board -> Maybe Color
checkMate = undefined
-- AI


rankValue :: Rank -> Int
rankValue Pawn  = 3
rankValue Queen = 50
rankValue King  = 900
rankValue _     = 25

valueBoard :: Board -> Int
valueBoard board = sum $ map (getPieceValue' board) $ getPiecePositions board Black ++ getPiecePositions board White

getPieceValue' :: Board -> Pos -> Int
getPieceValue' board (row,col)  
   | c == White = (-1) * rankValue r
   | r /= Pawn  = rankValue r 
   | otherwise  = rankValue r
      where Just (Piece r c) = board #!> (row,col)

getPiecePositions :: Board -> Color -> [Pos]
getPiecePositions (Board b) color' = getPieceInRows b 0
   where
      getPieceInRows :: [[Square]] -> Int -> [Pos]
      getPieceInRows [] _     = []
      getPieceInRows (r:rw) n = [(n,c) | c <- findIndices isJust r, (color $ fromJust ((Board b) #!> (n,c))) == color'] ++
                                getPieceInRows rw (n+1)

--
type Move = (Pos,Pos)

data MoveTree = Node Move [MoveTree] deriving Show

toLists :: MoveTree -> [[Move]]
toLists (Node move []) = [[move]]
toLists (Node m ms) = map (m:) $ concat $ map toLists ms

scoreList :: Board -> [Move] -> Int
scoreList board moves' = valueBoard resBoard
   where resBoard = foldl (\b m -> fromJust $ movePiece b m) board moves'

getRoot :: MoveTree -> Move
getRoot (Node m _) = m

genMoves :: Board -> Pos -> [Move]
genMoves board p = zip (repeat p) (possibleDest board p)

bruteForce' :: Board -> Int -> [(Move,Int)]
bruteForce' board n = calcScore listOfMoves `using` parList rdeepseq
   where
      calcScore   = map (\l -> ((head.head) l,score l))
      listOfMoves = map toLists $ plantTree board n
      score :: [[Move]] -> Int
      score = maximum.(map (scoreList board))

-- BruteForce best move from depth n
bruteForce :: Board -> Int -> Move
bruteForce board n = getTop sortedForest
   where sortedForest = sortBy (\(s1,_) (s2,_) -> compare s2 s1) scores
         scores = map (\t -> (scoreTree board t,t)) forest
         forest = plantTree board n
         getTop = getRoot.snd.head 

scoreTree :: Board -> MoveTree -> Int
scoreTree board (Node m []) = let Just board' = movePiece board m in 
                                 valueBoard board'
scoreTree board (Node m ms) = let Just board' = movePiece board m in
                                 maximum $ map (scoreTree board') ms


-- Create new level of tree from a Move
evalNodes :: Board -> Move -> Int -> [MoveTree]
evalNodes board move n = plantTree newBoard (n-1)
   where
      newBoard = fromJust $ movePiece board move

-- Create a List of Nodes from all availibe moves
-- N specifies depth
plantTree :: Board -> Int -> [MoveTree]
plantTree _ 0 = []
plantTree board n = [Node move (evalNodes board move n) | p <- pos, move <- (genMoves board p)]
  where pos = if n `mod` 2 == 0 then getPiecePositions board Black else getPiecePositions board White 
--Get all ai piece positions

--Test Main
main = do
   putStrLn $ show $ bruteForce' initBoard 4

{-
    -- various test functions, delete later
    vizDest :: Board -> Pos -> Board
    vizDest board pos = foldr (\p b -> b #=> (p,Just(Piece Queen White))) board $ possibleDest board pos

    testBoard = Board $ replicate 3 emptyRow ++
                (replicate 3 Nothing ++ Just (Piece Queen Black) : replicate 4 Nothing) :
                replicate 4 emptyRow
-}

-- Pure code ends here

                    


