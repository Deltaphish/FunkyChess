module Types where

import Data.List
import Test.QuickCheck

type Pos = (Int,Int)

type Direction = (Int,Int)

type Move = (Pos,Pos)

checkMove :: (Pos -> Bool) -> Move -> Bool
checkMove f mov = f (fst mov) && f (snd mov)

data InputResult = InvalidMove | ValidMove Flag Board deriving Show

data Flag = Non | Check Color | Checkmate Color deriving (Show, Eq)

maybeFlagColor :: Flag -> Maybe Color
maybeFlagColor Non          = Nothing
maybeFlagColor (Check c)     = Just c
maybeFlagColor (Checkmate c) = Just c


data Color = White | Black deriving Eq

opponent :: Color -> Color
opponent White = Black
opponent Black = White

data Rank =
    Pawn   |
    Tower  |
    Knight |
    Bishop |
    Queen  |
    King deriving Eq

data Piece = Piece{ rank :: Rank
                  , color :: Color
                  } deriving Eq

instance Arbitrary Piece where
   arbitrary = do r' <- r; c' <- c; return $ Piece r' c'
      where c = elements [White,Black]
            r = elements [Pawn,Tower,Knight,Bishop,Queen,King]

type Square = Maybe Piece

newtype Board = Board [[Square]]

instance Arbitrary Board where
   arbitrary = do matrix <- sequence $ replicate 8 row; return $ Board matrix
      where row = vectorOf 8 $ frequency [(25, arbitrary),(75,return Nothing)]

instance Show Color
   where
    show White = "White"
    show Black = "Black"

instance Show Rank
   where
    show Pawn    = "Pawn"
    show Knight  = "Knight"
    show Bishop  = "Bishop"
    show Tower   = "Tower"
    show Queen   = "Queen"
    show King    = "King"

instance Show Piece
   where
    show (Piece Pawn Black)   = "♙"
    show (Piece Knight Black) = "♘"
    show (Piece Bishop Black) = "♗"
    show (Piece Tower Black)  = "♖"
    show (Piece Queen Black)  = "♕"
    show (Piece King Black)   = "♔"

    show (Piece Pawn White)   = "♟"
    show (Piece Knight White) = "♞"
    show (Piece Bishop White) = "♝"
    show (Piece Tower White)  = "♜"
    show (Piece Queen White)  = "♛"
    show (Piece King White)   = "♚"

instance Show Board where
   show (Board matrix) = createGrid (map showRow matrix) ++ "  A B C D E F G H\n"
      where 
         createGrid = unlines.addNumberCol.addFloor
 
         addFloor :: [String] -> [String]
         addFloor = intersperse (init (concat (replicate 8 "— ")) ++ "|")
 
         addNumberCol :: [String] -> [String]
         addNumberCol = map (\(a,b) -> a ++ "|" ++ b) . zip (intersperse " " $ map show $ reverse [1..8])
 
         showRow [] = ""
         showRow ((Just p):row') = show p ++ "|" ++ showRow row'
         showRow (Nothing:row') = " |" ++ showRow row'