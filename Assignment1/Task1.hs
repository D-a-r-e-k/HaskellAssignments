module Task1
where

import TicTacToe.Messages.SExpr
  
data FutureMove = FutureMove {
      playerToMove :: Char
} deriving Show

data Move = Move {
      posX :: Int
    , posY :: Int
    , player :: Char
} deriving Show

type Moves = [Move]

parse :: String -> Moves
parse ('(':'l':rest) = 
   case checkEmptyList (readSeparator rest) of
     True -> []
     _ -> reverse $ parseTuples [] rest
parse _ = error "List is expected."

checkEmptyList :: String -> Bool
checkEmptyList (')':_) = True
checkEmptyList _       = False

readChar :: String -> String
readChar ('\"': 'x' : '\"': rest) = rest
readChar ('\"': 'y' : '\"': rest) = rest
readChar ('\"': 'v' : '\"': rest) = rest
readChar _ = error "Unsupported char"

readDigit :: String -> (Int, String)
readDigit ('0':rest) = (0, rest)
readDigit ('1':rest) = (1, rest) 
readDigit ('2':rest) = (2, rest) 
readDigit _ = error "Digit expected" 

readPlayer :: String -> (Char, String)
readPlayer ('\"': 'x' : '\"': rest) = ('x', rest)
readPlayer ('\"': 'o' : '\"': rest) = ('o', rest)
readPlayer ('\"': 'X' : '\"': rest) = ('x', rest)
readPlayer ('\"': 'O' : '\"': rest) = ('o', rest)
readPlayer _ = error "Player expected"

readSeparator :: String -> String
readSeparator (' ':rest) = readSeparator rest
readSeparator (el:rest) = [el] ++ rest

parseTuples :: Moves -> String -> Moves
parseTuples acc ")" = acc
parseTuples acc rest =
  let
    sepRest = readSeparator rest
    (tuple, restt) = parseTuple sepRest
    sep7Rest = readSeparator restt
  in
    parseTuples (tuple:acc) sep7Rest

parseTuple :: String 
           -> (Move, String)
parseTuple ('(':'m':rest) =
 let
   sep1Rest = readSeparator rest
   restxName = readChar sep1Rest
   sep2Rest = readSeparator restxName
   (x, restx) = readDigit sep2Rest
   sep3Rest = readSeparator restx
   restyName = readChar sep3Rest
   sep4Rest = readSeparator restyName
   (y, resty) = readDigit sep4Rest
   sep5Rest = readSeparator resty
   figRest = readChar sep5Rest
   sep6Rest = readSeparator figRest
   (player, restPlayer) = readPlayer sep6Rest
 in
   case restPlayer of
    (')':t) -> (Move x y player, t)
    _ -> error "Tuple without closing bracket"

countPlayerMoves :: Moves -> Char -> Int
countPlayerMoves moves p = length (filter (\x -> player x == p) moves)

isTableFull :: Moves -> Bool
isTableFull moves = (countPlayerMoves moves 'x') + (countPlayerMoves moves 'o') == 9

checkRow :: Moves -> Int -> Char -> Bool
checkRow moves row p = length (filter (\x -> posX x == row && player x /= p) moves) == 2 &&
                       length (filter (\x -> posX x == row && player x == p) moves) == 0

checkCol :: Moves -> Int -> Char -> Bool
checkCol moves col p = length (filter (\x -> posY x == col && player x /= p) moves) == 2 &&
                       length (filter (\x -> posY x == col && player x == p) moves) == 0

checkDiagMain :: Moves -> Char -> Bool
checkDiagMain moves p = length (filter (\x -> posX x == posY x && player x /= p) moves) == 2 &&
                        length (filter (\x -> posX x == posY x && player x == p) moves) == 0

checkDiagSecond :: Moves -> Char -> Bool
checkDiagSecond moves p = length (filter (\x -> (posX x == 0 && posY x == 2 && player x /= p) ||
                                                (posX x == 2 && posY x == 0 && player x /= p) || 
                                                (posX x == 1 && posY x == 1 && player x /= p)) moves) == 2 &&
                          length (filter (\x -> (posX x == 0 && posY x == 2 && player x == p) ||
                                                (posX x == 2 && posY x == 0 && player x == p) || 
                                                (posX x == 1 && posY x == 1 && player x == p)) moves) == 0

isEmptyCell :: Moves -> Int -> Int -> Bool
isEmptyCell moves x y = length (filter (\e -> posX e == x && posY e == y) moves) == 0

hasToMove :: Moves -> FutureMove
hasToMove moves
   | xMovesCount < oMovesCount = FutureMove 'x'
   | oMovesCount < xMovesCount = FutureMove 'o'
   | otherwise                 = FutureMove 'x'
   where xMovesCount = countPlayerMoves moves 'x'
         oMovesCount = countPlayerMoves moves 'o'

makeMove :: String -> Maybe Move
makeMove encodedMoves = makeMoveImpl moves (hasToMove moves)
   where moves = parse encodedMoves

getFirst (a, _) = a
getSecond (_, a) = a   

getEmptyCell :: Moves -> FutureMove -> [Int] -> [Int] -> Move
getEmptyCell moves movingPlayer xRange yRange =
  (map (\t -> Move (getFirst t) (getSecond t) (playerToMove movingPlayer))
       [(i,j) | i <- xRange, j <- yRange, isEmptyCell moves i j])!!0
   
makeMoveImpl :: Moves -> FutureMove -> Maybe Move
makeMoveImpl moves movingPlayer
   | isFull    = Nothing
   | row0      = Just (getEmptyCell moves movingPlayer [0] [0..2])
   | row1      = Just (getEmptyCell moves movingPlayer [1] [0..2])
   | row2      = Just (getEmptyCell moves movingPlayer [2] [0..2])
   | col0      = Just (getEmptyCell moves movingPlayer [0..2] [0])
   | col1      = Just (getEmptyCell moves movingPlayer [0..2] [1])
   | col2      = Just (getEmptyCell moves movingPlayer [0..2] [2])
   | diag0     = Just ((filter (\e -> isEmptyCell moves (posX e) (posY e))
                               (map (\t -> Move (getFirst t) (getSecond t) (playerToMove movingPlayer))
                                    [(0,0),(1,1),(2,2)]))!!0)
   | diag1     = Just ((filter (\e -> isEmptyCell moves (posX e) (posY e))
                               (map (\t -> Move (getFirst t) (getSecond t) (playerToMove movingPlayer))
                                    [(0,2),(1,1),(2,0)]))!!0)
   | otherwise = Just (getEmptyCell moves movingPlayer [0..2] [0..2])
   where isFull = isTableFull moves
         row0 = checkRow moves 0 (playerToMove movingPlayer)
         row1 = checkRow moves 1 (playerToMove movingPlayer)
         row2 = checkRow moves 2 (playerToMove movingPlayer)
         col0 = checkCol moves 0 (playerToMove movingPlayer)
         col1 = checkCol moves 1 (playerToMove movingPlayer)
         col2 = checkCol moves 2 (playerToMove movingPlayer)
         diag0 = checkDiagMain moves (playerToMove movingPlayer)
         diag1 = checkDiagSecond moves (playerToMove movingPlayer)  