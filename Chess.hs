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

newtype Board = Board [[Square]]

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
         addNumberCol = map (\(a,b) -> a ++ "|" ++ b).(zip (intersperse " " $ map show $ reverse [1..8]))

         showRow [] = ""
         showRow ((Just p):row') = show p ++ "|" ++ showRow row'
         showRow (Nothing:row') = " |" ++ showRow row'

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
-- This does not account for moves requiring special scenarios

moves :: Piece -> [Direction]
moves (Piece Pawn White) = [(-1,0)]
moves (Piece Pawn Black) = [(1,0)]
moves (Piece Tower _ )   = [(1,0),(-1,0),(0,1),(0,-1)]
moves (Piece Knight _ )  = [(-2,1),(-2,-1),(-1,2),(-1,-2),
                           (2,1),(2,-1),(1,2),(1,-2)]
moves (Piece Bishop _ )  = [(1,1),(1,-1),(-1,1),(-1,-1)]
moves (Piece Queen _)    = moves (Piece Tower White) ++ moves (Piece Bishop White) -- Color of no importance
moves (Piece King _)     = [(dr',dc') | dr' <- [-1..1], dc' <- [-1..1], (dr',dc') /= (0,0)]


onBoard :: Pos -> Bool
onBoard (r,c) = r >= 0 && r <= 7 && c >= 0 && c <= 7

{-
-- Special version when we want to check if a piece is covering a friendly piece
possibleDest' :: Board -> Pos -> [Pos]
possibleDest' board startPos
   | (rank p) == Pawn                   = [(r+dr,c+dc) | dc <- [-1..1]]
   | (rank p) `elem` [Knight,King]      = concat [ walk startPos move False | move <- moves p]
   | otherwise                          = concat [ walk startPos move True  | move <- moves p]
   whereF
      p = fromMaybe (error ("possibleDest': no piece at position " ++ show startPos))
             (board #!> startPos)

      (r,c) = startPos
      (dr,_) = head $ moves p

      walk :: Pos -> Direction -> Bool -> [Pos]
      walk (r,c) (dr,dc) b
         | not $ onBoard newPos = []
         | otherwise = case board #!> newPos of
                          Nothing -> if b then newPos : walk newPos (dr,dc) b else [newPos]
                          Just p' -> [newPos]
            where newPos = (r+dr,c+dc)
-}

-- Generates a list of possible destinations for the piece at startPos
possibleDest :: Board -> Bool -> Pos -> [Pos]
possibleDest board checkCoverage startPos
   | (rank p) == Pawn                   = pawnWalk
   | (rank p) `elem` [Knight,King]      = concat [ walk startPos move False | move <- moves p]
   | otherwise                          = concat [ walk startPos move True  | move <- moves p]
   where
      p = fromMaybe (error ("possibleDest: no piece at position " ++ show startPos))
             (board #!> startPos)

      -- Special case for pawn since pawn can only go diagonal if it can take a piece and two steps if first step
      pawnWalk :: [Pos]
      pawnWalk = canForward ++ canTwoStep ++ concatMap canDiagonal [-1,1]
         where
            (dr,_) = head $ moves p
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
      walk (r,c) (dr,dc) b
         | not $ onBoard newPos = []
         | otherwise = case board #!> newPos of
                          Nothing -> if b then newPos : walk newPos (dr,dc) b else [newPos]
                          Just p' -> if checkCoverage || color p' == color p
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
   | dest `elem` possibleDest board False start = Just $ (board #=> (dest,board #!> start)) #=> (start,Nothing)
   | otherwise                                  = error "Invalid Move"

-- Returns the color of the player who has been checkmated
checkMate :: Board -> Maybe Color
checkMate = undefined
-- AI


rankValue :: Rank -> Int
rankValue Pawn   = 1
rankValue Knight = 3
rankValue Bishop = 4
rankValue Tower  = 5
rankValue Queen  = 8
rankValue King   = 999 

valueBoard :: Board -> Int
valueBoard board = pieceValueSum + coverageSum
   where 
      pieceValueSum = sum $ map (getPieceValue' board) $ getPiecePositions board Black ++ getPiecePositions board White
      coverageSum   = getCoverValue board

-- Make the ai value moves that cover its own pieces
getCoverValue :: Board -> Int
getCoverValue board = length $ intersect destinations (getPiecePositions board Black)
   where destinations = concatMap (possibleDest board True) (getPiecePositions board Black)

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
genMoves board p = zip (repeat p) (possibleDest board False p)

bruteForce' :: Board -> Int -> Move
bruteForce' board n = fst $ maximumBy (\(_,s1) (_,s2) -> compare s1 s2) parCalc
   where
      parCalc     = (calcScore listOfMoves `using` parList rdeepseq)
      calcScore   = map (\l -> ((head.head) l,score l))
      listOfMoves = map toLists $ createMoveTree board n
      score :: [[Move]] -> Int
      score = maximum.(map (scoreList board))

scoreTree :: Board -> MoveTree -> Int
scoreTree board (Node m []) = let Just board' = movePiece board m in 
                                 valueBoard board'
scoreTree board (Node m ms) = let Just board' = movePiece board m in
                                 maximum $ map (scoreTree board') ms

scoreMoves :: Board -> [Move] -> [(Int,Move)]
scoreMoves _ []         = []
scoreMoves board (m:ms) = case movePiece board m of
                             Just board' -> (valueBoard board',m) : (scoreMoves board ms)
                             Nothing     -> error ("scoreMoves: illeagal move " ++ show m)


{-
   Evaluate the ai's next move by creating a tree of all possible moves and their results.
   Then pick the tree with the greatest score in the lowest node. To make use of parallellization,
   we convert all tree to lists. 
-}

-- Create new level of tree from a Move
evalMoveTree :: Board -> Move -> Int -> [MoveTree]
evalMoveTree board move n = createMoveTree newBoard (n-1)
   where
      newBoard = case movePiece board move of
                    Just board' -> board'
                    Nothing     -> error ("newBoard: illeagal move " ++ show move)

-- Create a List of Nodes from all availibe moves
-- N specifies depth
createMoveTree :: Board -> Int -> [MoveTree]
createMoveTree _ 0 = []
createMoveTree board n | n `mod` 2 == 0 = [Node move (evalMoveTree board move n) | p <- pos Black, move <- (genMoves board p)]
                     | otherwise      = [Node bestPlayerMove (evalMoveTree board bestPlayerMove n)]
                     where pos = getPiecePositions board
                           playerMoves = concat $ filter (not . null) $ map (genMoves board) (pos White)
                           bestPlayerMove = snd $ minimumBy (\(s1,_) (s2,_) -> compare s1 s2) $ scoreMoves board playerMoves
--Get all ai piece positions

makeAiMove board = fromMaybe (error "makeAiMove: invalid move") 
   (movePiece board (bruteForce' board 6))

--Test Main
main = do
   let board1 = iterate makeAiMove initBoard
   putStrLn $ (show $ take 10 board1)
   

   --ToDo assume i will play a smart move

{-
    -- various test functions, delete later
    vizDest :: Board -> Pos -> Board
    vizDest board pos = foldr (\p b -> b #=> (p,Just(Piece Queen White))) board $ possibleDest board pos

    testBoard = Board $ replicate 3 emptyRow ++
                (replicate 3 Nothing ++ Just (Piece Queen Black) : replicate 4 Nothing) :
                replicate 4 emptyRow
-}

-- Pure code ends here

                    


