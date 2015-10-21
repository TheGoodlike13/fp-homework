module Main where

import Data.Maybe
import JsonParser
import TicTacToe

validate :: String -> Bool
validate json = fromMaybe False (fmap isValid (parsedMoves json))

move :: String -> Maybe (Int, Int, Char)
move json = (parsedMoves json) >>= nextMove >>= convert

winner :: String -> Maybe Char
winner json = fmap convertToken (board >>= gameWinner)
    where
        board = fmap justBoard (parsedMoves json >>= replay)

parsedMoves :: String -> Maybe MoveHistory
parsedMoves = parseJson

convert :: Move -> Maybe (Int, Int, Char)
convert (Move (x, y, token))
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | otherwise = Just (x, y, convertToken token)

convertToken :: Token -> Char
convertToken token = head (show token)
