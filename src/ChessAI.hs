module ChessAI where

import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad.Par
import Control.DeepSeq
import Test.QuickCheck

import Types
import Board

-- Recursive Tree Datatype for collecting the moves generated for the ai


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
   | c == playerColor = 10 * negate (rankValue r)
   | otherwise        =          rankValue r
      where Just (Piece r c) = board #!> (row,col)

valueBoard :: Board -> Int
valueBoard board = pieceValueSum + coverageSum
   where 
      pieceValueSum = sum $ map (getPieceValue' board) $ 
                        getPiecePositions board aiColor ++
                        getPiecePositions board playerColor
      coverageSum   = getCoverValue board

{-
-- Check that bruteforce does not crash for valid boards + all moves are on the board 
prop_validAiMove :: Board -> Property
prop_validAiMove board = length (blacks) > 1 &&
                       length (whites) > 1 &&
                       length (allMoves blacks) > 2 &&
                       length (allMoves whites) > 2 ==> let m = bruteForce 2 board in checkMove onBoard m
                       where
                         blacks = getPiecePositions board Black
                         whites = getPiecePositions board White
                         allMoves = concatMap (possibleMoves board)
-}

{-
   Evaluate the ai's next move by creating a trees of all possible move-chains from the board.
   Then pick the tree with the greatest score in the lowest node. To make use of parallellization,
   we convert all trees to lists of move-chains.
-}

type Score = Int
data MoveTree = Root Board [MoveTree] | Node Board Score Move [MoveTree] deriving Show
instance NFData MoveTree where
   rnf _ = ()


valuateNode :: Board -> Move -> (Board,Score)
valuateNode b m | isCheck b' playerColor = (b',-9999999)
               | otherwise            = (b',valueBoard b')
    where b' = movePiece b m

createMoveTree b n = 
   runPar $ do
     let moves = allPossibleMoves b aiColor
     let scoredMoves = map (\m -> (valuateNode b m,m)) moves
     children <- parMap (\((b',s'),m') -> createMoveTreeLayer b' s' m' (n-1)) scoredMoves
     return $ Root b children

createMoveTreeLayer :: Board -> Score -> Move -> Int -> MoveTree
createMoveTreeLayer board score move 0 = Node board score move []
createMoveTreeLayer oldboard score move n = 
   runPar $ do
      let board = movePiece oldboard (bestPlayerMove oldboard)
      let moves = allPossibleMoves board aiColor
      evalMoves <- parMap (valuateNode board) moves
      children <- parMap (\(b',s') -> createMoveTreeLayer b' s' move (n-1)) evalMoves 
      return $ Node board score move children

getScores (Node _ s m [] ) = [(s,m)]
getScores (Node _ _ _ nds) = concatMap getScores nds


getBestMove :: MoveTree -> Move
getBestMove (Root _ xs) = snd $ maximumBy (comparing fst) $ concatMap getScores xs

bestPlayerMove board = snd $ minimumBy (\((_,s1),_) ((_,s2),_) -> compare s1 s2) $ zip (map (valuateNode board) playerMoves) playerMoves
   where 
      pos = getPiecePositions board
      playerMoves = concat $ filter (not . null) $ map (possibleMoves board) (pos playerColor)

makeAiMove b = [fst move, snd move]
   where move = getBestMove $ createMoveTree b 3

-- Create a List of Nodes from all availibe moves
-- N specifies depth
{-
createMoveTree :: Board -> Int -> [MoveTree]
createMoveTree _ 0 = []
createMoveTree board n | n `mod` 2 == 0 = [Node move (evalMoveTree board move n) | p <- pos aiColor, move <- possibleMoves board p]
                       | otherwise      = [Node bestPlayerMove (evalMoveTree board bestPlayerMove n)]
                       where
                             
-}
