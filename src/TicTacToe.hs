module TicTacToe where

import Data.List
import Data.Maybe
import JsonParser

data Token = Cross | Circle deriving (Eq)
data Move = Move (Int, Int, Token) deriving (Eq)
data MoveHistory = MoveHistory [Move] deriving (Eq)
data Board = Board [[Maybe Token]] deriving (Eq)

instance Show Token where
    show Cross = "X"
    show Circle = "O"

instance Show Move where
    show (Move (x, y, token)) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show token ++ ")"

instance Show MoveHistory where
    show (MoveHistory moves) = "[" ++ intercalate ", " [show move | move <- moves] ++ "]"

instance Show Board where
    show (Board board) = showBoard board
    show (Board []) = "\n+-----+"
    show (Board (row : others)) = "\n+-----+\n" ++ show row ++ show (Board others)

showBoard :: [[Maybe Token]] -> String
showBoard [] = "\n+-----+"
showBoard (row : others) = "\n+-----+\n" ++ rowString ++ showBoard others
    where
        rowString = case row of
            [] -> "|"
            (Nothing : others) ->





