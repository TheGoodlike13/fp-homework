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

showBoard :: [[Maybe Token]] -> String
showBoard [] = "\n+-----+"
showBoard (row : others) = "\n+-----+\n" ++ showRow row ++ showBoard others

showRow :: [Maybe Token] -> String
showRow [] = "|"
showRow (Nothing : others) = "| " ++ showRow others
showRow ((Just token) : others) = "|" ++ show token ++ showRow others

instance Json Token where
    toJson token = JsonString (show token)
    fromJson (JsonString v)
        | v == "x" || v == "X" = Just Cross
        | v == "o" || v == "O" || v == "0" = Just Circle
    fromJson _ = Nothing

instance Json Move where
    toJson (Move (x, y, token)) = JsonObject [jsonX, jsonY, jsonV]
        where
            jsonX = JsonPair ("x", JsonInt x)
            jsonY = JsonPair ("y", JsonInt y)
            jsonV = JsonPair ("v", toJson token)
    fromJson (JsonObject [JsonPair ("x", JsonInt x), JsonPair ("y", JsonInt y), JsonPair ("v", v)])
        = fmap (\t -> Move (x, y, t)) (fromJson v)
    fromJson _ = Nothing

instance Json MoveHistory where
    toJson (MoveHistory moves) = JsonObject [JsonPair (show i, toJson jsonMove) | jsonMove <- moves, i <- [0..length moves]]
    fromJson (JsonObject moves) = accumulateJson moves []

accumulateJson :: [JsonPair] -> [Move] -> Maybe MoveHistory
accumulateJson [] mem = Just (MoveHistory (reverse mem))
accumulateJson (JsonPair (_, jsonMove) : others) mem = fromJson jsonMove >>= (\move -> accumulateJson others (move : mem))

emptyBoard :: Board
emptyBoard = Board (replicate 3 (replicate 3 Nothing))

defaultFirstMove :: Move
defaultFirstMove = Move (1, 1, Cross)

getNextPlayer :: Board -> Token
getNextPlayer (Board board)
    | (nothingCount board) `mod` 2 == 1 = Cross
    | otherwise = Circle











