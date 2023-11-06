import Data.List

type Board = [[[Int]]]

--ok: ja foram testadas

--verificar linha?
--ok
nextCell :: (Int, Int) -> Int -> (Int, Int)
nextCell (row, col) size
    | col < size - 1 = (row, col + 1)
    | otherwise = (row + 1, 0)


--ok
isFull :: [[[Int]]] -> Bool
isFull board = all (\x -> length x == 1) (concat board)


--ok
boardCopy :: [[[Int]]] -> [[[Int]]]
boardCopy [[[]]] = [[[]]]
boardCopy board = map (map (map id)) board

-- neighbors :: [[[Int]]] -> [[String]] -> Int -> Int -> (Int, Int, Int, Int)
-- neighbors board sign_board row column = (top_num, right_num, bottom_num, left_num)

--ok
neighborsSigns :: [[String]] -> Int -> Int -> (Char, Char, Char, Char)
neighborsSigns signBoard row column = str
  where
    str = stringToTuple (show (signBoard !! row !! column))

-- Função para verificar se o tabuleiro é válido
isValid :: Board -> Bool
isValid board =
    let size = length board
        regionSize = round (sqrt (fromIntegral size))
    in isFull board && checkRows board size && checkColumns board size && checkBlocks board size regionSize

--checkRows, checkColumns e checkBlocks sao funcoes auxiliares para isValid

-- Função para verificar numero repetido nas linhas
--ok
checkRows :: Board -> Int -> Bool
checkRows board size = all (\i -> checkRow (sort [x | x <- board !! i])) [0 .. size - 1]
  where
    checkRow [] = True
    checkRow [_] = True
    checkRow (x:y:xs)
        | x == y = False
        | otherwise = checkRow (y:xs)

-- Função para verificar numero repetido nas colunas
--ok
checkColumns :: Board -> Int -> Bool
checkColumns board size = all (\i -> checkColumn (sort [board !! j !! i | j <- [0 .. size - 1]])) [0 .. size - 1]
  where
    checkColumn [] = True
    checkColumn [_] = True
    checkColumn (x:y:xs)
        | x == y = False
        | otherwise = checkColumn (y:xs)

-- Função para verificar numero repetido nos blocos
--ok
checkBlocks :: Board -> Int -> Int -> Bool
checkBlocks board size regionSize = all (\i -> all (\j -> checkBlock (sort [board !! x !! y | x <- [i..i+regionSize-1], y <- [j..j+regionSize-1]])) [0, regionSize .. size - 1]) [0, regionSize .. size - 1]
  where
    checkBlock [] = True
    checkBlock [_] = True
    checkBlock (x:y:xs)
        | x == y = False
        | otherwise = checkBlock (y:xs)

--testar
isPlacementValid :: [[[Int]]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Char -> Char -> Char -> Char -> Bool
isPlacementValid board row column value size regionSize topNum rightNum bottomNum leftNum top right bottom left =
    let isRowValid = all (\(i, element) -> length element /= 1 || (element !! 0) /= value || i == column) (zip [0..] (board !! row))
        isColumnValid = all (\i -> length (board !! i !! column) /= 1 || (board !! i !! column !! 0) /= value || i == row) [0..size-1]
        regionRow = row `div` regionSize
        regionColumn = column `div` regionSize
        isRegionValid = all (\i -> all (\j -> length (board !! i !! j) /= 1 || (board !! i !! j !! 0) /= value || (i, j) == (row, column)) [regionColumn*regionSize..(regionColumn+1)*regionSize-1]) [regionRow*regionSize..(regionRow+1)*regionSize-1]
        areSignsValid = signValid top right bottom left value topNum rightNum bottomNum leftNum
    in isRowValid && isColumnValid && isRegionValid && areSignsValid



--testar
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

--ok
printBoard :: [[[Int]]] -> IO ()
printBoard board = do
    putStrLn "Tabuleiro:"
    putStrLn "-------------------------"
    mapM_ printRow board
    putStrLn "-------------------------"
    putStrLn ""
  where
    printRow :: [[Int]] -> IO ()
    printRow row = do
        putStr "| "
        mapM_ (\x -> putStr (show (x!!0) ++ " ")) row
        putStrLn "|"

--ok
countPops :: Char -> Char -> Char -> Char -> (Int, Int)
countPops top right bottom left = (popFront, popBack)
  where
    popFront = countSignal '^' top + countSignal '>' right + countSignal 'v' bottom + countSignal '<' left
    popBack = countSignal 'v' top + countSignal '<' right + countSignal '^' bottom + countSignal '>' left

    countSignal :: Char -> Char -> Int
    countSignal s signal = if signal == s then 1 else 0

--ok
vergleichPreprocess :: [[[Int]]] -> [[String]] -> [[[Int]]]
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


--preProcess :: [[[Int]]] -> [[String]] -> [[[Int]]]

--solve 

--ok
printCountPossibilities :: [[[Int]]] -> IO ()
printCountPossibilities board = do
    putStrLn "Matriz de possibilidades:"
    putStrLn "-------------------------"
    mapM_ printRow board
    putStrLn "-------------------------"
    putStrLn ""
  where
    printRow :: [[Int]] -> IO ()
    printRow row = do
        putStr "| "
        mapM_ (\x -> putStr (show (length x) ++ " ")) row
        putStrLn "|"

--ok
make_board :: Int -> [[[Int]]]
make_board 0 = [[[]]]
make_board size = replicate size $ replicate size [1..size]


-- Função para verificar se o tabuleiro está cheio
--eh igual a de cima, as duas foram testadas e funcionam
--deixei caso alguma das duas dê problema

-- isFull :: Board -> Bool
-- isFull board = all (\row -> all (\cell -> length cell == 1) row) board





--ok
stringToTuple :: String -> (Char, Char, Char, Char)
stringToTuple str = (str !! 0, str !! 1, str !! 2, str !! 3)





--ok
getCell :: [[[Int]]] -> (Int, Int, Int) -> Int
getCell board (x, y, z) = (board !! x) !! y !! z

--ok
isSizeEqual :: Int -> [cell] -> Bool
isSizeEqual size list = length list == size




main :: IO ()
main = do
    let size = 9
        board =  make_board size
        board2 = [
                [[5], [3], [4], [6], [7], [8], [9], [1], [2]],
                [[6], [7], [2], [1], [9], [5], [3], [4], [8]],
                [[1], [9], [8], [3], [4], [2], [5], [6], [7]],
                [[8], [5], [9], [7], [6], [1], [4], [2], [3]],
                [[4], [2], [6], [8], [5], [3], [7], [9], [1]],
                [[7], [1], [3], [9], [2], [4], [8], [5], [6]],
                [[9], [6], [1], [5], [3], [7], [2], [8], [4]],
                [[2], [8], [7], [4], [1], [9], [6], [3], [5]],
                [[3], [4], [5], [2], [8], [6], [1], [7], [9]]
                ]
        sign_board = [ 
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

    -- printCountPossibilities board
    -- printMatrix board2
    -- putStrLn (show (isFull board2))
    -- putStrLn (show (isFull board))
    -- putStrLn (show (neighborsSigns sign_board 0 0))
    -- putStrLn (show (nextCell (0, 0) size))
    -- putStrLn (show (getCell board2 (0, 0, 0)))
    -- printBoard board2
    -- putStrLn (show (neighbors board2 sign_board 2 3))
    -- putStrLn (show (countPops 'v' '>' 'v' '<'))
    printCountPossibilities (vergleichPreprocess board sign_board)
    -- putStrLn (show (isSizeEqual 1 [1]))
    -- putStrLn (show (isSizeEqual 3 [1, 2]))
    -- putStrLn (show (isSizeEqual 2 [1, 2]))

