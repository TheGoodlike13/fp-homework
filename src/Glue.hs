module Glue where

import JSON
import TicTacToe

import Data.Maybe

validate :: String -> Bool
validate json = isJust parsed && isValid moves
    where
        parsed = parseJson json
        moves = fromJust parsed

move :: String -> Maybe (Int, Int, Char)
move json = fmap parseMove move
    where
        parsed = parseJson json
        move = parsed >>= nextMove

winner :: String -> Maybe Char
winner json = fmap parseToken winnerToken
    where
        parsed = parseJson json
        winnerToken = parsed >>= victorious

-- converters

parseJson :: String -> Maybe MoveHistory
parseJson json = parse json >>= parseMovesFromValues

parseMovesFromValues :: JsonValue -> Maybe MoveHistory
parseMovesFromValues (JsonObject pairs) = accumulateMovesFromPairs pairs []

accumulateMovesFromPairs :: [JsonPair] -> MoveHistory -> Maybe MoveHistory
accumulateMovesFromPairs [] history = Just (reverse history)
accumulateMovesFromPairs ((_, value) : others) history
    | isJust move = accumulateMovesFromPairs others (fromJust move : history)
    | otherwise = Nothing
    where
        move = parseMoveFromValue value

parseMoveFromValue :: JsonValue -> Maybe Move
parseMoveFromValue (JsonObject [("x", JsonInt x), ("y", JsonInt y), ("v", JsonString v)])
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | v == "x" || v == "X" = Just (x, y, Cross)
    | v == "o" || v == "O" || v == "0" = Just (x, y, Circle)
    | otherwise = Nothing
parseMoveFromValue _ = Nothing

parseMove :: Move -> (Int, Int, Char)
parseMove (x, y, token) = (x, y, parseToken token)

parseToken :: Token -> Char
parseToken Cross = 'X'
parseToken Circle = 'O'
