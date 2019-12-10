module ChessAI (makeAiMove,testAi) where

import Data.List
import Data.Maybe
import Control.Parallel.Strategies

import Board

-- Recursive Tree Datatype for collecting the moves generated for the ai
data MoveTree = Node Move [MoveTree]

getRoot :: MoveTree -> Move
getRoot (Node m _) = m

-- Converts MoveTree To lists of move-chains for easier parallelization 
toMoveChains :: MoveTree -> [[Move]]
toMoveChains (Node move []) = [[move]]
toMoveChains (Node m ms) = map (m:) $ concatMap toMoveChains ms


{--- Constants ---}
playerColor = White
aiColor     = Black

{--- Functions for scoring boards ---}
rankValue :: Rank -> Int
rankValue Pawn   = 1
rankValue Knight = 3
rankValue Bishop = 4
rankValue Tower  = 5
rankValue Queen  = 8
rankValue King   = 999 

-- Make the ai value moves that cover its own pieces
getCoverValue :: Board -> Int
getCoverValue board = length $ intersect destinations (getPiecePositions board aiColor)
   where destinations = concatMap (possibleDest board True) (getPiecePositions board aiColor)

getPieceValue' :: Board -> Pos -> Int
getPieceValue' board (row,col)  
   | c == playerColor = 10 * (negate $ rankValue r)
   | otherwise        =          rankValue r
      where Just (Piece r c) = board #!> (row,col)

valueBoard :: Board -> Int
valueBoard board = pieceValueSum + coverageSum
   where 
      pieceValueSum = sum $ map (getPieceValue' board) $ 
                        getPiecePositions board aiColor ++
                        getPiecePositions board playerColor
      coverageSum   = getCoverValue board


scoreMoveChain :: Board -> [Move] -> Int
scoreMoveChain board moves' = valueBoard resBoard
   where resBoard = foldl (\b m -> fromJust $ movePiece b m) board moves'

{--- Functions for generating/evaluating moveTrees ---}


-- decides a move by generating a tree of all possible moves to a level n, scoring and choosing the best one

bruteForce :: Int -> Board -> Move
bruteForce n board = fst $ maximumBy (\(_,s1) (_,s2) -> compare s1 s2) parCalc
   where
      parCalc     = calcScore listOfMoves `using` parList rdeepseq
      calcScore   = map (\l -> ((head.head) l,score l))
      listOfMoves = map toMoveChains $ createMoveTree board n
      score :: [[Move]] -> Int
      score = maximum . map (scoreMoveChain board)

scoreMoves :: Board -> [Move] -> [(Int,Move)]
scoreMoves _ []         = []
scoreMoves board (m:ms) = case movePiece board m of
                             Just board' -> (valueBoard board',m) : scoreMoves board ms
                             Nothing     -> error ("scoreMoves: illeagal move " ++ show m)


{-
   Evaluate the ai's next move by creating a trees of all possible move-chains from the board.
   Then pick the tree with the greatest score in the lowest node. To make use of parallellization,
   we convert all trees to lists of move-chains.
-}

-- Create new level of tree from a Move
evalMoveTree :: Board -> Move -> Int -> [MoveTree]
evalMoveTree board move n = createMoveTree newBoard (n-1)
   where
      newBoard = fromMaybe (error ("newBoard: illeagal move " ++ show move)) 
                    (movePiece board move)

-- Create a List of Nodes from all availibe moves
-- N specifies depth
createMoveTree :: Board -> Int -> [MoveTree]
createMoveTree _ 0 = []
createMoveTree board n | n `mod` 2 == 0 = [Node move (evalMoveTree board move n) | p <- pos aiColor, move <- possibleMoves board p]
                       | otherwise      = [Node bestPlayerMove (evalMoveTree board bestPlayerMove n)]
                       where pos = getPiecePositions board
                             playerMoves = concat $ filter (not . null) $ map (possibleMoves board) (pos playerColor)
                             bestPlayerMove = snd $ minimumBy (\(s1,_) (s2,_) -> compare s1 s2) $ scoreMoves board playerMoves

-- Wrapper for external use
makeAiMove :: Board -> Board
makeAiMove board = fromMaybe (error "makeAiMove: invalid move") 
                      (movePiece board (bruteForce 6 board))

testAi :: (Board -> Board) -> Board -> Int -> IO()
testAi engine board n = do
   let moveSequence = iterate engine board
   print (take n moveSequence)

--Test Main
main = testAi (makeAiMove) initBoard 10

{-
    -- various test functions, delete later
    vizDest :: Board -> Pos -> Board
    vizDest board pos = foldr (\p b -> b #=> (p,Just(Piece Queen White))) board $ possibleDest board pos

    testBoard = Board $ replicate 3 emptyRow ++
                (replicate 3 Nothing ++ Just (Piece Queen Black) : replicate 4 Nothing) :
                replicate 4 emptyRow
-}

-- Pure code ends here

                    


