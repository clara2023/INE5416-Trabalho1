import Data.List
import Control.Monad

-- Definição de tipos de dados
type Board = [[[Int]]]
type SignBoard = [[String]]

-- Função para verificar se o tabuleiro é válido
isValid :: Board -> Bool
isValid board =
    let size = length board
        regionSize = round (sqrt (fromIntegral size))
    in isFull board && checkRows board size && checkColumns board size && checkBlocks board size regionSize

-- Função para verificar se o tabuleiro está cheio
isFull :: Board -> Bool
isFull board = all (\row -> all (\cell -> length cell == 1) row) board

-- Função para verificar as linhas
checkRows :: Board -> Int -> Bool
checkRows board size = all (\i -> checkRow (sort [x | x <- board !! i])) [0 .. size - 1]
  where
    checkRow [] = True
    checkRow [_] = True
    checkRow (x:y:xs)
        | x == y = False
        | otherwise = checkRow (y:xs)

-- Função para verificar as colunas
checkColumns :: Board -> Int -> Bool
checkColumns board size = all (\i -> checkColumn (sort [board !! j !! i | j <- [0 .. size - 1]])) [0 .. size - 1]
  where
    checkColumn [] = True
    checkColumn [_] = True
    checkColumn (x:y:xs)
        | x == y = False
        | otherwise = checkColumn (y:xs)

-- Função para verificar os blocos
checkBlocks :: Board -> Int -> Int -> Bool
checkBlocks board size regionSize = all (\i -> all (\j -> checkBlock (sort [board !! x !! y | x <- [i..i+regionSize-1], y <- [j..j+regionSize-1]])) [0, regionSize .. size - 1]) [0, regionSize .. size - 1]
  where
    checkBlock [] = True
    checkBlock [_] = True
    checkBlock (x:y:xs)
        | x == y = False
        | otherwise = checkBlock (y:xs)

vergleichPreprocess :: Board -> SignBoard -> Board
vergleichPreprocess board signBoard =
    let size = length board
        copy = boardCopy board  -- Copia do tabuleiro original
        processRow :: [[Int]] -> [(Char, Char, Char, Char)] -> [[Int]]
        processRow row pops = [processCell cell pop | (cell, pop) <- zip row pops]
        pops = [[stringToTuple (signBoard !! row !! column) | column <- [0..size-1]] | row <- [0..size-1]]
        processCell :: [Int] -> (Char, Char, Char, Char) -> [Int]
        processCell cell (top, right, bottom, left) =
            let (popFront, popBack) = countPops top right bottom left
            in drop popFront $ reverse $ drop popBack $ reverse cell
    in [processRow row rowPops | (row, rowPops) <- zip copy pops]

nextCell :: (Int, Int) -> Int -> (Int, Int)
nextCell (row, col) size
    | col < size - 1 = (row, col + 1)
    | otherwise = (row + 1, 0)

countPops :: Char -> Char -> Char -> Char -> (Int, Int)
countPops top right bottom left = (popFront, popBack)
  where
    popFront = countSignal '^' top + countSignal '>' right + countSignal 'v' bottom + countSignal '<' left
    popBack = countSignal 'v' top + countSignal '<' right + countSignal '^' bottom + countSignal '>' left

    countSignal :: Char -> Char -> Int
    countSignal s signal = if signal == s then 1 else 0

make_board :: Int -> Board
make_board 0 = [[[]]]
make_board size = replicate size $ replicate size [1..size]

boardCopy :: Board -> Board
boardCopy [[[]]] = [[[]]]
boardCopy board = map (map (map id)) board

neighborsSigns :: SignBoard -> Int -> Int -> (Char, Char, Char, Char)
neighborsSigns signBoard row column = str
  where
    str = stringToTuple (show (signBoard !! row !! column))

stringToTuple :: String -> (Char, Char, Char, Char)
stringToTuple str = (str !! 0, str !! 1, str !! 2, str !! 3)

signValid :: Char -> Char -> Char -> Char -> Int -> Int -> Int -> Int -> Int -> Bool
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
isPlacementValid board signBoard row column value = checkRow && checkColumn && checkBlock && checkSigns
  where
    size = length board
    regionSize = floor (sqrt (fromIntegral size))

    -- Função para verificar a validade da linha
    checkRow = not $ any (\(i, element) -> i /= column && length element == 1 && head element == value) $ zip [0..] (board !! row)

    -- Função para verificar a validade da coluna
    checkColumn = not $ any (\i -> i /= row && length (board !! i !! column) == 1 && head (board !! i !! column) == value) [0..size - 1]

    -- Função para verificar a validade do bloco
    blockRow = row `div` regionSize
    blockColumn = column `div` regionSize
    checkBlock = not $ any (\(row1, column1) ->
        let indices = [(i, j) | i <- [blockRow * regionSize..(blockRow + 1) * regionSize - 1], j <- [blockColumn * regionSize..(blockColumn + 1) * regionSize - 1], i /= row || j /= column]
        in
            any (\(i, j) -> length (board !! i !! j) == 1 && head (board !! i !! j) == value) indices
        ) [(row1, column1) | row1 <- [blockRow * regionSize..(blockRow + 1) * regionSize - 1], column1 <- [blockColumn * regionSize..(blockColumn + 1) * regionSize - 1]]

    -- Função para verificar a validade dos sinais
    (top, right, bottom, left) = stringToTuple (signBoard !! row !! column)
    (topNeighbor, rightNeighbor, bottomNeighbor, leftNeighbor) = neighbors board signBoard row column
    checkSigns = signValid top right bottom left value topNeighbor rightNeighbor bottomNeighbor leftNeighbor

validateAllPlacements :: Board -> SignBoard -> [(Int, Int, Int, Bool)]
validateAllPlacements board signBoard =
    [(x, y, z, isPlacementValid board signBoard x y z) | x <- [0..size-1], y <- [0..size-1], z <- [1..size]]
  where
    size = length board

solve :: Board -> SignBoard -> Int -> Int -> Maybe Board
solve board signBoard row column
    | isFull board =
        if isValid board
            then Just board
            else Nothing
    | otherwise = case nextCell (row, column) (length board) of
        (nextRow, nextColumn) ->
            if null (board !! row !! column)
                then Nothing
                else tryPossibilities board signBoard row column nextRow nextColumn (board !! row !! column)

tryPossibilities :: Board -> SignBoard -> Int -> Int -> Int -> Int -> [Int] -> Maybe Board
tryPossibilities board signBoard row column nextRow nextColumn [] = Nothing
tryPossibilities board signBoard row column nextRow nextColumn (i:rest) =
    let copy = board
        updatedCopy = replaceCell copy row column [i]
    in if isPlacementValid updatedCopy signBoard row column i
        then case solve updatedCopy signBoard nextRow nextColumn of
            Just result -> Just result
            Nothing -> tryPossibilities board signBoard row column nextRow nextColumn rest
        else tryPossibilities board signBoard row column nextRow nextColumn rest

replaceCell :: Board -> Int -> Int -> [Int] -> Board
replaceCell board rowIndex colIndex newValue =
    take rowIndex board ++
    [take colIndex (board !! rowIndex) ++ [newValue] ++ drop (colIndex + 1) (board !! rowIndex)] ++
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
            | column < size - 1 && right == '<' = maximum (board !! row !! (column + 1))
            | column < size - 1 && right == '>' = minimum (board !! row !! (column + 1))
            | otherwise = num

        updateBottom num
            | row < size - 1 && bottom == '^' = maximum (board !! (row + 1) !! column)
            | row < size - 1 && bottom == 'v' = minimum (board !! (row + 1) !! column)
            | otherwise = num

        updateLeft num
            | column > 0 && left == '>' = maximum (board !! row !! (column - 1))
            | column > 0 && left == '<' = minimum (board !! row !! (column - 1))
            | otherwise = num
    in
        (updateTop topNum, updateRight rightNum, updateBottom bottomNum, updateLeft leftNum)

-- Função principal
main :: IO ()
main = do
    let sign_board = [ 
                    [".>v.", ".<v>", "..v<",      ".<^.", ".<v<", "..v<",      ".>^.", ".<^>", "..^<"],
                    ["v>^.", "v<v>", "v.v<",      "^>^.", "v<^>", "v.v<",      "^>v.", "^<^>", "^.v<"],
                    ["^>..", "v<.>", "v..<",      "^>..", "^>.>", "v..>",      "v>..", "^<.>", "v..<"],

                    [".<v.", ".>v<", "..v>",      ".<^.", ".<^<", "..^<",      ".<^.", ".<^<", "..v<"],
                    ["v<^.", "v>^<", "v.v>",      "^<v.", "^<v<", "^.v<",      "^<^.", "^>v<", "v.^>"],
                    ["^<..", "^>.<", "v..>",      "v<..", "v>.<", "v..>",      "^<..", "v<.<", "^..<"],

                    [".>v.", ".>v>", "..^>",      ".>^.", ".<^>", "..^<",      ".>v.", ".<v>", "..v<"],
                    ["v>^.", "v<^>", "^.v<",      "^<^.", "^>v<", "^.v>",      "v<v.", "v>^<", "v.^>"],
                    ["^<..", "^>.<", "v..>",      "^>..", "v>.>", "v..>",      "v<..", "^>.<", "^..>"]
                    ]

    let new_board = vergleichPreprocess (make_board 9) sign_board
    let possibleResult = solve new_board sign_board 0 0
    case possibleResult of
        Just result -> print result
        Nothing -> putStrLn "No solution found"
