module TicTacToe where

import Data.Maybe

data Token = Cross | Circle
type Move = (Int, Int, Token)
type MoveHistory = [Move]
type Row = [Maybe Token]
type Board = [Row]
type GameState = (Board, Token)

isValid :: MoveHistory -> Bool
isValid moves = isJust (replay moves)

nextMove :: MoveHistory -> Maybe Move
nextMove moves
    | length moves == 9 = Nothing
    | length moves == 0 = Just defaultFirstMove
    | otherwise = (replay moves) >>= findNextMove

victorious :: MoveHistory -> Maybe Token
victorious moves = board >>= weHaveAWinner
    where
        board = fmap justBoard (replay moves)

-- toStr

toStr :: Board -> String
toStr [] = "+-----+\n"
toStr (row : others) =  "+-----+\n" ++ toStr2 row ++ toStr others

toStr2 :: Row -> String
toStr2 [] = "|\n"
toStr2 (token : others) = "|" ++ toStr3 token ++ toStr2 others

toStr3 :: Maybe Token -> String
toStr3 Nothing = " "
toStr3 (Just Cross) = "X"
toStr3 (Just Circle) = "O"

toStr4 :: GameState -> String
toStr4 (board, nextPlayer) = toStr board

-- helpers

freshBoard :: Board
freshBoard = replicate 3 (replicate 3 Nothing)

initialGameState :: GameState
initialGameState = (freshBoard, Cross)

justBoard :: GameState -> Board
justBoard (board, nextPlayer) = board

defaultFirstMove :: Move
defaultFirstMove = (1, 1, Cross)

play :: Move -> GameState -> Maybe GameState
play (x, y, token) (board, nextPlayer)
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | isJust (weHaveAWinner board) = Nothing
    | same token nextPlayer = newState
    | otherwise = Nothing
    where
        oldRow = board !! x
        newRow = replaceToken y token oldRow
        newBoard = fmap (\row -> replace x row board) newRow
        newState = makeState newBoard nextPlayer

replay :: MoveHistory -> Maybe GameState
replay moves = replayAccumulate moves (Just initialGameState)

findNextMove :: GameState -> Maybe Move
findNextMove (board, nextPlayer)
    | checkWin (other nextPlayer) board = Nothing
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
overtake (x, y, token) = (x, y, other token)

findWinningMoves :: [Move] -> Board -> [Move]
findWinningMoves [] _ = []
findWinningMoves (move : others) board
    | isJust winner = move : findWinningMoves others board
    | otherwise = findWinningMoves others board
    where
        afterMove = forceMove move board
        winner = weHaveAWinner afterMove

findAllPossibleMoves :: Board -> Token -> [Move]
findAllPossibleMoves board token = accumulatePossibleMoves board token 0 []

accumulatePossibleMoves :: Board -> Token -> Int -> [Move] -> [Move]
accumulatePossibleMoves [] _ _ moves = moves
accumulatePossibleMoves (row : rest) token x moves = accumulatePossibleMoves rest token (x + 1) (movesInRow ++ moves)
    where
        movesInRow = accumulatePossibleMovesInRow row token x 0 []

accumulatePossibleMovesInRow :: Row -> Token -> Int -> Int -> [Move] -> [Move]
accumulatePossibleMovesInRow [] _ _ _ moves = moves
accumulatePossibleMovesInRow (first : rest) token x y moves
    | isJust first = accumulatePossibleMovesInRow rest token x (y + 1) moves
    | otherwise = accumulatePossibleMovesInRow rest token x (y + 1) (move : moves)
    where
        move = (x, y, token)

weHaveAWinner :: Board -> Maybe Token
weHaveAWinner board
    | checkWin Cross board = Just Cross
    | checkWin Circle board = Just Circle
    | otherwise = Nothing

other :: Token -> Token
other Cross = Circle
other Circle = Cross

same :: Token -> Token -> Bool
same Cross Cross = True
same Circle Circle = True
same _ _ = False

replayAccumulate :: MoveHistory -> Maybe GameState -> Maybe GameState
replayAccumulate [] state = state
replayAccumulate (move : others) state = replayAccumulate others (state >>= play move)

replaceToken :: Int -> Token -> Row -> Maybe Row
replaceToken i token row
    | i < 0 || i >= length row = Nothing
    | isJust currentItem = Nothing
    | otherwise = Just (replace i (Just token) row)
    where
        currentItem = row !! i

makeState :: Maybe Board -> Token -> Maybe GameState
makeState Nothing _ = Nothing
makeState board token = Just (fromJust board, other token)

checkWin :: Token -> Board -> Bool
checkWin token board = checkRows token board || checkColumns token board || checkDiagonals token board

checkRows :: Token -> Board -> Bool
checkRows _ [] = False
checkRows token (first : others)
    | checkArray token first = True
    | otherwise = checkRows token others

checkColumns :: Token -> Board -> Bool
checkColumns token board = checkColumnsStartingWith 0 token board

checkColumnsStartingWith :: Int -> Token -> Board -> Bool
checkColumnsStartingWith i token board
    | i >= length board = False
    | checkArray token (map (\row -> row !! i) board) = True
    | otherwise = checkColumnsStartingWith (i + 1) token board

checkDiagonals :: Token -> Board -> Bool
checkDiagonals token board = checkArray token (leftDiagonal board) || checkArray token (rightDiagonal board)

leftDiagonal :: Board -> [Maybe Token]
leftDiagonal board = diagonalAccumulate board [] 0 (\i -> i + 1)

rightDiagonal :: Board -> [Maybe Token]
rightDiagonal board = diagonalAccumulate board [] (length board - 1) (\i -> i - 1)

diagonalAccumulate :: Board -> [Maybe Token] -> Int -> (Int -> Int) -> [Maybe Token]
diagonalAccumulate [] diagonal _ _ = diagonal
diagonalAccumulate (row : others) diagonal i f_i = diagonalAccumulate others ((row !! i) : diagonal) (f_i i) f_i

checkArray :: Token -> [Maybe Token] -> Bool
checkArray _ [] = True
checkArray token (first : others)
    | isNothing first = False
    | same token (fromJust first) = checkArray token others
    | otherwise = False

forceMove :: Move -> Board -> Board
forceMove (x, y, token) board = newBoard
    where
        oldRow = board !! x
        newRow = replace y (Just token) oldRow
        newBoard = replace x newRow board

-- utils

replace :: Int -> a -> [a] -> [a]
replace i element (first : others)
    | i < 0 || i >= length others + 1 = error "Invalid index"
    | i == 0 = element : others
    | otherwise = first : replace (i - 1) element others

-- test

testBoard :: Board
testBoard = forceMove (1, 1, Cross) freshBoard

testState :: GameState
testState = (testBoard, Circle)

pb :: Board -> IO()
pb board = pmb (Just board)

ps :: GameState -> IO()
ps state = pms (Just state)

pr :: Row -> IO()
pr row = pmr (Just row)

pt :: Token -> IO()
pt token = pmt (Just token)

pm :: Move -> IO()
pm move = pmm (Just move)

pmb :: Maybe Board -> IO()
pmb Nothing = putStr "Nothing\n"
pmb (Just board) = putStr (toStr board)

pms :: Maybe GameState -> IO()
pms Nothing = putStr "Nothing\n"
pms (Just (board, nextPlayer)) = putStr (toStr board)

pmr :: Maybe Row -> IO()
pmr Nothing = putStr "Nothing\n"
pmr (Just row) = putStr (toStr2 row)

pmt :: Maybe Token -> IO()
pmt token = putStr ((toStr3 token) ++ "\n")

pmm :: Maybe Move -> IO()
pmm Nothing = putStr "Nothing\n"
pmm (Just (x, y, token)) = putStr ("(" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (toStr3 (Just token)) ++ ")\n")
