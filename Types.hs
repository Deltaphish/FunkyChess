module Types where

import Data.List

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

instance Show Piece
   where
    show (Piece Pawn White)   = "♙"
    show (Piece Knight White) = "♘"
    show (Piece Bishop White) = "♗"
    show (Piece Tower White)  = "♖"
    show (Piece Queen White)  = "♕"
    show (Piece King White)   = "♔"

    show (Piece Pawn Black)   = "♟"
    show (Piece Knight Black) = "♞"
    show (Piece Bishop Black) = "♝"
    show (Piece Tower Black)  = "♜"
    show (Piece Queen Black)  = "♛"
    show (Piece King Black)   = "♚"

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