import Control.Monad
import Data.List

-- Definição de tipos de dados
type Board = [[[Int]]]

type SignBoard = [[String]]

-- Função para verificar se o tabuleiro é válido
isValid :: Board -> Bool
isValid board =
  isFull board && checkRows board && checkColumns board && checkBlocks board

-- Função para verificar se o tabuleiro está cheio
isFull :: Board -> Bool
isFull board = all (\row -> all (\cell -> length cell == 1) row) board

-- Função para verificar as linhas
checkRows :: Board -> Bool
checkRows board =
  all (\i -> checkRow (sort [x | x <- board !! i])) [0 .. size - 1]
  where
    size = length board
    checkRow [] = True
    checkRow [_] = True
    checkRow (x:y:xs)
      | x == y = False
      | otherwise = checkRow (y : xs)

-- Função para verificar as colunas
checkColumns :: Board -> Bool
checkColumns board =
  all
    (\i -> checkColumn (sort [board !! j !! i | j <- [0 .. size - 1]]))
    [0 .. size - 1]
  where
    size = length board
    checkColumn [] = True
    checkColumn [_] = True
    checkColumn (x:y:xs)
      | x == y = False
      | otherwise = checkColumn (y : xs)

-- Função para verificar os blocos
checkBlocks :: Board -> Bool
checkBlocks board =
  all
    (\i ->
       all
         (\j ->
            checkBlock
              (sort
                 [ board !! x !! y
                 | x <- [i .. i + regionSizeY - 1]
                 , y <- [j .. j + regionSizeX - 1]
                 ]))
         [0,regionSizeX .. size - 1])
    [0,regionSizeY .. size - 1]
  where
    size = length board
    regionSizeX = floor (sqrt (fromIntegral size))
    regionSizeY = size `div` regionSizeX
    checkBlock [] = True
    checkBlock [_] = True
    checkBlock (x:y:xs)
      | x == y = False
      | otherwise = checkBlock (y : xs)

vergleichPreprocess :: Board -> SignBoard -> Board
vergleichPreprocess board signBoard =
  let size = length board
      processRow :: [[Int]] -> [(Char, Char, Char, Char)] -> [[Int]]
      processRow row pops = [processCell cell pop | (cell, pop) <- zip row pops]
      pops =
        [ [ stringToTuple (signBoard !! row !! column)
        | column <- [0 .. size - 1]
        ]
        | row <- [0 .. size - 1]
        ]
      processCell :: [Int] -> (Char, Char, Char, Char) -> [Int]
      processCell cell (top, right, bottom, left) =
        let (popFront, popBack) = countPops top right bottom left
         in drop popFront $ reverse $ drop popBack $ reverse cell
   in [processRow row rowPops | (row, rowPops) <- zip board pops]

nextCell :: (Int, Int) -> Int -> (Int, Int)
nextCell (row, col) size
  | col < size - 1 = (row, col + 1)
  | otherwise = (row + 1, 0)

countPops :: Char -> Char -> Char -> Char -> (Int, Int)
countPops top right bottom left = (popFront, popBack)
  where
    popFront =
      countSignal '^' top + countSignal '>' right + countSignal 'v' bottom +
      countSignal '<' left
    popBack =
      countSignal 'v' top + countSignal '<' right + countSignal '^' bottom +
      countSignal '>' left
    countSignal :: Char -> Char -> Int
    countSignal s signal =
      if signal == s
        then 1
        else 0

make_board :: Int -> Board
make_board 0 = [[[]]]
make_board size = replicate size $ replicate size [1 .. size]

neighborsSigns :: SignBoard -> Int -> Int -> (Char, Char, Char, Char)
neighborsSigns signBoard row column = stringToTuple (show (signBoard !! row !! column))

stringToTuple :: String -> (Char, Char, Char, Char)
stringToTuple str = (str !! 0, str !! 1, str !! 2, str !! 3)

signValid ::
     Char -> Char -> Char -> Char -> Int -> Int -> Int -> Int -> Int -> Bool
signValid top right bottom left number topNum rightNum bottomNum leftNum
  | top == '^' && topNum /= 0 && number <= topNum = False
  | top == 'v' && topNum /= 0 && number >= topNum = False
  | right == '>' && rightNum /= 0 && number <= rightNum = False
  | right == '<' && rightNum /= 0 && number >= rightNum = False
  | bottom == 'v' && bottomNum /= 0 && number <= bottomNum = False
  | bottom == '^' && bottomNum /= 0 && number >= bottomNum = False
  | left == '<' && leftNum /= 0 && number <= leftNum = False
  | left == '>' && leftNum /= 0 && number >= leftNum = False
  | otherwise = True

isPlacementValid :: Board -> SignBoard -> Int -> Int -> Int -> Bool
isPlacementValid board signBoard row column value =
  checkRow && checkColumn && checkBlock && checkSigns
  where
    size = length board
    regionSizeX = floor (sqrt (fromIntegral size))
    regionSizeY = size `div` regionSizeX
    -- Função para verificar a validade da linha
    checkRow =
      not $
      any
        (\(i, element) ->
           i /= column && length element == 1 && head element == value) $
      zip [0 ..] (board !! row)
    -- Função para verificar a validade da coluna
    checkColumn =
      not $
      any
        (\i ->
           i /= row &&
           length (board !! i !! column) == 1 &&
           head (board !! i !! column) == value)
        [0 .. size - 1]
    -- Função para verificar a validade do bloco
    blockRow = row `div` regionSizeY
    blockColumn = column `div` regionSizeX
    checkBlock =
      not $
      any
        (\(row1, column1) ->
           let indices =
                 [ (i, j)
                 | i <-
                     [blockRow * regionSizeY .. (blockRow + 1) * regionSizeY - 1]
                 , j <-
                     [blockColumn * regionSizeX .. (blockColumn + 1) *
                      regionSizeX -
                      1]
                 , i /= row || j /= column
                 ]
            in any
                 (\(i, j) ->
                    length (board !! i !! j) == 1 &&
                    head (board !! i !! j) == value)
                 indices)
        [ (row1, column1)
        | row1 <- [blockRow * regionSizeY .. (blockRow + 1) * regionSizeY - 1]
        , column1 <-
            [blockColumn * regionSizeX .. (blockColumn + 1) * regionSizeX - 1]
        ]
    -- Função para verificar a validade dos sinais
    (top, right, bottom, left) = stringToTuple (signBoard !! row !! column)
    (topNeighbor, rightNeighbor, bottomNeighbor, leftNeighbor) =
      neighbors board signBoard row column
    checkSigns =
      signValid
        top
        right
        bottom
        left
        value
        topNeighbor
        rightNeighbor
        bottomNeighbor
        leftNeighbor

solve :: Board -> SignBoard -> Int -> Int -> Maybe Board
solve board signBoard row column
  | isFull board =
    if isValid board
      then Just board
      else Nothing
  | otherwise =
    case nextCell (row, column) (length board) of
      (nextRow, nextColumn) ->
        if null (board !! row !! column)
          then Nothing
          else tryPossibilities
                 board
                 signBoard
                 row
                 column
                 nextRow
                 nextColumn
                 (board !! row !! column)

tryPossibilities ::
     Board -> SignBoard -> Int -> Int -> Int -> Int -> [Int] -> Maybe Board
tryPossibilities board signBoard row column nextRow nextColumn [] = Nothing
tryPossibilities board signBoard row column nextRow nextColumn (i:rest) =
  let updatedBoard = replaceCell board row column [i]
   in if isPlacementValid updatedBoard signBoard row column i
        then case solve updatedBoard signBoard nextRow nextColumn of
               Just result -> Just result
               Nothing ->
                 tryPossibilities
                   board
                   signBoard
                   row
                   column
                   nextRow
                   nextColumn
                   rest
        else tryPossibilities board signBoard row column nextRow nextColumn rest

replaceCell :: Board -> Int -> Int -> [Int] -> Board
replaceCell board rowIndex colIndex newValue =
  take rowIndex board ++
  [ take colIndex (board !! rowIndex) ++
    [newValue] ++ drop (colIndex + 1) (board !! rowIndex)
  ] ++
  drop (rowIndex + 1) board

neighbors :: Board -> SignBoard -> Int -> Int -> (Int, Int, Int, Int)
neighbors board signBoard row column =
  let size = length board
      (top, right, bottom, left) = stringToTuple (signBoard !! row !! column)
      (topNum, rightNum, bottomNum, leftNum) = (0, 0, 0, 0)
      updateTop num
        | row > 0 && top == 'v' = maximum (board !! (row - 1) !! column)
        | row > 0 && top == '^' = minimum (board !! (row - 1) !! column)
        | otherwise = num
      updateRight num
        | column < size - 1 && right == '<' =
          maximum (board !! row !! (column + 1))
        | column < size - 1 && right == '>' =
          minimum (board !! row !! (column + 1))
        | otherwise = num
      updateBottom num
        | row < size - 1 && bottom == '^' =
          maximum (board !! (row + 1) !! column)
        | row < size - 1 && bottom == 'v' =
          minimum (board !! (row + 1) !! column)
        | otherwise = num
      updateLeft num
        | column > 0 && left == '>' = maximum (board !! row !! (column - 1))
        | column > 0 && left == '<' = minimum (board !! row !! (column - 1))
        | otherwise = num
   in ( updateTop topNum
      , updateRight rightNum
      , updateBottom bottomNum
      , updateLeft leftNum)

printBoard :: Board -> IO ()
printBoard board = do
    let size = length board
        regionSizeX = floor (sqrt (fromIntegral size))
        regionSizeY = size `div` regionSizeX
    printBoardRows board size regionSizeX regionSizeY

printBoardRows :: Board -> Int -> Int -> Int -> IO ()
printBoardRows _ 0 _ _ = return ()
printBoardRows board size regionSizeX regionSizeY = do
    printRow (head board) regionSizeX
    putStrLn ""
    when ((size-1) `mod` regionSizeY == 0) $ putStrLn ""
    printBoardRows (tail board) (size - 1) regionSizeX regionSizeY

printRow :: [[Int]] -> Int -> IO ()
printRow [] _ = return ()
printRow row regionSizeX = do
    printCell (head row)
    when (((length row) -1) `mod` regionSizeX == 0) $ putStr " "
    printRow (tail row) regionSizeX

printCell :: [Int] -> IO ()
printCell cell
    | length cell /= 1 = putStr "□ "
    | otherwise = putStr (show (head cell) ++ " ")


example :: Int -> SignBoard
example x
  | x == 1 = [
  [".<v.", "..^<",    ".<^.", "..v<"],
  ["v<..", "^..<",    "^>..", "v..>"],

  [".>v.", "..^>",    ".<^.", "..v<"],
  ["v>..", "^..>",    "^>..", "v..>"]
            ]
  | x == 23 = [
  [".>v.", "..v>",   ".>^.", "..v>",   ".<^.", "..v<"],
  ["v<v.", "v.^<",   "^>v.", "v.^>",   "^<v.", "v.v<"],
  ["v<..", "^..<",   "v>..", "^..>",   "v<..", "v..<"],

  [".<^.", "..v<",   ".<^.", "..v<",   ".>^.", "..^>"],
  ["^>v.", "v.^>",   "^<^.", "v.^<",   "^>^.", "^.v>"],
  ["v>..", "^..>",   "^<..", "^..<",   "^>..", "v..>"]
                ]
  | x == 60 = [
  [".<v.", ".<v<", "..v<",   ".<^.", ".<^<", "..v<",   ".<^.", ".<v<", "..v<"],
  ["v<^.", "v<v<", "v.^<",   "^>v.", "^>v>", "v.^>",   "^>^.", "v<^>", "v.v<"],
  ["^>..", "v<.>", "^..<",   "v>..", "v<.>", "^..<",   "^>..", "^<.>", "v..<"],

  [".<v.", ".>v<", "..^>",   ".<^.", ".>v<", "..^>",   ".<^.", ".>v<", "..^>"],
  ["v<^.", "v>v<", "^.^>",   "^>v.", "v<v>", "^.v<",   "^>^.", "v<^>", "^.^<"],
  ["^<..", "v>.<", "^..>",   "v<..", "v>.<", "v..>",   "^>..", "^>.>", "^..>"],

  [".>v.", ".<v>", "..v<",   ".>^.", ".<^>", "..^<",   ".>v.", ".<v>", "..v<"],
  ["v>^.", "v>^>", "v.^>",   "^<v.", "^>v<", "^.v>",   "v>v.", "v>^>", "v.v>"],
  ["^<..", "^>.<", "^..>",   "v<..", "v>.<", "v..>",   "v<..", "^>.<", "v..>"]
              ]
  | otherwise = example 1

-- Função principal
main :: IO ()
main = do
  -- Exemplos: 1, 23, 60
  -- (2x2), (6x6), (9x9)
  -- example <n> para escolher um dos tabuleiros
  let sign_board = example 60
  
  let new_board = vergleichPreprocess (make_board (length sign_board)) sign_board
  let possibleResult = solve new_board sign_board 0 0
  case possibleResult of
    Just result -> printBoard result
    Nothing -> putStrLn "No solution found"
