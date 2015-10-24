module Main where

import Data.List
import Data.Maybe
import JsonParser
import Network.HTTP
import System.Environment
import TicTacToe

validate :: String -> Bool
validate json = fromMaybe False (fmap isValid (parsedMoves json))

move :: String -> Maybe (Int, Int, Char)
move json = parsedMoves json >>= replay >>= nextMove >>= convert

winner :: String -> Maybe Char
winner json = fmap convertToken (parsedMoves json >>= replay >>= gameWinner)

parsedMoves :: String -> Maybe MoveHistory
parsedMoves = parseJson

convert :: Move -> Maybe (Int, Int, Char)
convert (x, y, token)
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | otherwise = Just (x, y, convertToken token)

convertToken :: Token -> Char
convertToken token = head (show token)

gameUrl :: String -> String
gameUrl gameId = "http://tictactoe.homedir.eu/game/" ++ gameId

contentType :: String
contentType = "application/json+map"

getGameBody :: String -> IO String
getGameBody url = do
    response <- simpleHTTP (getRequest url)
    getResponseBody response

postGameMove :: String -> Move -> IO String
postGameMove url move = do
    response <- simpleHTTP (postRequestWithBody url contentType (encodeJson move))
    getResponseBody response

playGame :: String -> IO()
playGame matchUrl = do
    movesJson <- getGameBody matchUrl
    let board = parseJson movesJson >>= replay
    let nextGameMove = board >>= nextMove
    if (isNothing nextGameMove) then
        print "Game over!" >> return ()
    else
        postGameMove matchUrl (fromJust nextGameMove) >> print (fromJust board) >> playGame matchUrl

main :: IO()
main = do
    matchName <- fmap head getArgs
    let matchUrl = gameUrl matchName
    playGame matchUrl

