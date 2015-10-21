module TicTacToe where

import Data.List
import Data.Maybe
import JsonParser

data Token = Cross | Circle deriving (Eq)
data Move = Move (Int, Int, Token) deriving (Eq)
data MoveHistory = MoveHistory [Move] deriving (Eq)
data Row = Row [Maybe Token] deriving (Eq)
data Board = Board [Row] deriving (Eq)
data GameState = GameState (Board, Token) deriving (Eq)

instance Show Token where
    show Cross = "X"
    show Circle = "O"

instance Show Move where
    show (Move (x, y, token)) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show token ++ ")"

instance Show MoveHistory where
    show (MoveHistory moves) = "[" ++ intercalate ", " [show move | move <- moves] ++ "]"

instance Show Row where
    show (Row []) = "|"
    show (Row (Nothing : others)) = "| " ++ show (Row others)
    show (Row ((Just token) : others)) = "|" ++ show token ++ show (Row others)

instance Show Board where
    show (Board []) = "\n+-----+"
    show (Board (row : others)) = "\n+-----+\n" ++ show row ++ show (Board others)

instance Show GameState where
    show (GameState (board, nextPlayer)) = show board ++ "\nNext player: " ++ show nextPlayer

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

isValid :: MoveHistory -> Bool
isValid moves = isJust (replay moves)

nextMove :: MoveHistory -> Maybe Move
nextMove moves
    | amountOf moves == 9 = Nothing
    | amountOf moves == 0 = Just defaultFirstMove
    | otherwise = (replay moves) >>= findNextMove

gameWinner :: Board -> Maybe Token
gameWinner board
    | isWinner Cross board = Just Cross
    | isWinner Circle board = Just Circle
    | otherwise = Nothing

emptyBoard :: Board
emptyBoard = Board (replicate 3 (Row (replicate 3 Nothing)))

getState :: Board -> GameState
getState board
    | (nothingCount board) `mod` 2 == 1 = GameState (board, Cross)
    | otherwise = GameState (board, Circle)

justBoard :: GameState -> Board
justBoard (GameState (board, _)) = board

defaultFirstMove :: Move
defaultFirstMove = Move (1, 1, Cross)

play :: Move -> GameState -> Maybe GameState
play (Move (x, y, token)) (GameState (board, nextPlayer))
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | winnerExists board = Nothing
    | token /= nextPlayer = Nothing
    | otherwise = newState
    where
        newBoard = applyMove x y token board
        newState = fmap getState newBoard

applyMove :: Int -> Int -> Token -> Board -> Maybe Board
applyMove x y token board = newBoard
    where
        newRow = replaceToken y token (getRow x board)
        newBoard = newRow >>= (\row -> replaceRow x row board)

replaceRow :: Int -> Row -> Board -> Maybe Board
replaceRow x row (Board board)
    | x < 0 || x >= length board = Nothing
    | otherwise = Just (Board (replace x row board))

replaceToken :: Int -> Token -> Row -> Maybe Row
replaceToken y token (Row row)
    | y < 0 || y >= length row = Nothing
    | isJust currentItem = Nothing
    | otherwise = Just (Row (replace y (Just token) row))
    where
        currentItem = row !! y

replay :: MoveHistory -> Maybe GameState
replay (MoveHistory moves) = replayAccumulate moves (Just (getState emptyBoard))

replayAccumulate :: [Move] -> Maybe GameState -> Maybe GameState
replayAccumulate [] state = state
replayAccumulate (move : others) state = replayAccumulate others (state >>= play move)

findNextMove :: GameState -> Maybe Move
findNextMove (GameState (board, nextPlayer))
    | isWinner (other nextPlayer) board = Nothing
    | haveAnyWinningMoves = Just (head winningMoves)
    | nextPlayerHasMoreThanOneWinningMove = Nothing
    | nextPlayerHasOneWinningMove = Just (overtake (head nextPlayerWinningMoves))
    | otherwise = Just (head possibleMoves)
    where
        possibleMoves = findAllPossibleMoves board nextPlayer
        winningMoves = findWinningMoves possibleMoves board
        haveAnyWinningMoves = (not . null) winningMoves
        otherPlayerMoves = map overtake possibleMoves
        nextPlayerWinningMoves = findWinningMoves otherPlayerMoves board
        nextPlayerHasOneWinningMove = length nextPlayerWinningMoves == 1
        nextPlayerHasMoreThanOneWinningMove = length nextPlayerWinningMoves > 1

overtake :: Move -> Move
overtake (Move (x, y, token)) = Move (x, y, other token)

findWinningMoves :: [Move] -> Board -> [Move]
findWinningMoves [] _ = []
findWinningMoves (move : others) board
    | winnerExists afterMove = move : findWinningMoves others board
    | otherwise = findWinningMoves others board
    where
        afterMove = forceMove move board

findAllPossibleMoves :: Board -> Token -> [Move]
findAllPossibleMoves (Board board) token = accumulatePossibleMoves board token 0 []

accumulatePossibleMoves :: [Row] -> Token -> Int -> [Move] -> [Move]
accumulatePossibleMoves [] _ _ moves = moves
accumulatePossibleMoves (Row row : rest) token x moves = accumulatePossibleMoves rest token (x + 1) (movesInRow ++ moves)
    where
        movesInRow = accumulatePossibleMovesInRow row token x 0 []

accumulatePossibleMovesInRow :: [Maybe Token] -> Token -> Int -> Int -> [Move] -> [Move]
accumulatePossibleMovesInRow [] _ _ _ moves = moves
accumulatePossibleMovesInRow (first : rest) token x y moves
    | isJust first = accumulatePossibleMovesInRow rest token x (y + 1) moves
    | otherwise = accumulatePossibleMovesInRow rest token x (y + 1) (move : moves)
    where
        move = Move (x, y, token)

winnerExists :: Board -> Bool
winnerExists board = isJust (gameWinner board)

other :: Token -> Token
other Cross = Circle
other Circle = Cross

isWinner :: Token -> Board -> Bool
isWinner token (Board board) = checkRows token board || checkColumns token board || checkDiagonals token board

checkRows :: Token -> [Row] -> Bool
checkRows _ [] = False
checkRows token ((Row row) : others)
    | checkArray token row = True
    | otherwise = checkRows token others

checkColumns :: Token -> [Row] -> Bool
checkColumns token board = checkColumnsStartingWith 0 token board

checkColumnsStartingWith :: Int -> Token -> [Row] -> Bool
checkColumnsStartingWith i token board
    | i >= length board = False
    | checkArray token (map (\row -> getToken i row) board) = True
    | otherwise = checkColumnsStartingWith (i + 1) token board

checkDiagonals :: Token -> [Row] -> Bool
checkDiagonals token board = checkArray token (leftDiagonal board) || checkArray token (rightDiagonal board)

leftDiagonal :: [Row] -> [Maybe Token]
leftDiagonal board = diagonalAccumulate board [] 0 (\i -> i + 1)

rightDiagonal :: [Row] -> [Maybe Token]
rightDiagonal board = diagonalAccumulate board [] (length board - 1) (\i -> i - 1)

diagonalAccumulate :: [Row] -> [Maybe Token] -> Int -> (Int -> Int) -> [Maybe Token]
diagonalAccumulate [] diagonal _ _ = diagonal
diagonalAccumulate (row : others) diagonal i f_i = diagonalAccumulate others ((getToken i row) : diagonal) (f_i i) f_i

checkArray :: Token -> [Maybe Token] -> Bool
checkArray _ [] = True
checkArray token (first : others)
    | isNothing first = False
    | token == (fromJust first) = checkArray token others
    | otherwise = False

forceMove :: Move -> Board -> Board
forceMove (Move (x, y, token)) board = fromJust newBoard
    where
        newBoard = applyMove x y token board

nothingCount :: Board -> Int
nothingCount (Board board) = sum [nothingCountInRow row | row <- board]

nothingCountInRow :: Row -> Int
nothingCountInRow (Row row) = length [token | token <- row, isNothing token]

replace :: Int -> a -> [a] -> [a]
replace i element (first : others)
    | i < 0 || i >= length others + 1 = error "Invalid index"
    | i == 0 = element : others
    | otherwise = first : replace (i - 1) element others

getRow :: Int -> Board -> Row
getRow i (Board board) = board !! i

getToken :: Int -> Row -> Maybe Token
getToken i (Row row) = row !! i

amountOf :: MoveHistory -> Int
amountOf (MoveHistory moves) = length moves
