import Data.List


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


isPlacementValid :: [[[Int]]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Char -> Char -> Char -> Char -> Bool
isPlacementValid board row column value size regionSize topNum rightNum bottomNum leftNum top right bottom left =
    let isRowValid = all (\(i, element) -> length element /= 1 || (element !! 0) /= value || i == column) (zip [0..] (board !! row))
        isColumnValid = all (\i -> length (board !! i !! column) /= 1 || (board !! i !! column !! 0) /= value || i == row) [0..size-1]
        regionRow = row `div` regionSize
        regionColumn = column `div` regionSize
        isRegionValid = all (\i -> all (\j -> length (board !! i !! j) /= 1 || (board !! i !! j !! 0) /= value || (i, j) == (row, column)) [regionColumn*regionSize..(regionColumn+1)*regionSize-1]) [regionRow*regionSize..(regionRow+1)*regionSize-1]
        areSignsValid = signValid top right bottom left value topNum rightNum bottomNum leftNum
    in isRowValid && isColumnValid && isRegionValid && areSignsValid

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

--ok
boardCopy :: [[[Int]]] -> [[[Int]]]
boardCopy [[[]]] = [[[]]]
boardCopy board = map (map (map id)) board

--ok
isFull :: [[[Int]]] -> Bool
isFull board = all (\x -> length x == 1) (concat board)

--ok
neighborsSigns :: [[String]] -> Int -> Int -> (Char, Char, Char, Char)
neighborsSigns signBoard row column = str
  where
    str = stringToTuple (show (signBoard !! row !! column))

--ok
stringToTuple :: String -> (Char, Char, Char, Char)
stringToTuple str = (str !! 0, str !! 1, str !! 2, str !! 3)

--verificar linha?

nextCell :: (Int, Int) -> Int -> (Int, Int)
nextCell (row, col) size
    | col < size - 1 = (row, col + 1)
    | otherwise = (row + 1, 0)

--ok
printSudoku :: [[[Int]]] -> IO ()
printSudoku board = do
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

-- printSudoku :: [[Int]] -> IO ()
-- printSudoku board = do
--     let numRows = length board
--         numCols = length (head board)
--     putStrLn (horizontalLine numCols)
--     mapM_ (printRow numCols) (zip [0..] board)
--     putStrLn (horizontalLine numCols)

--   where
--     horizontalLine :: Int -> String
--     horizontalLine numCols = replicate (4 * numCols + 1) '-'

--     printRow :: Int -> (Int, [Int]) -> IO ()
--     printRow numCols (rowIndex, row) = do
--         putStr "| "
--         mapM_ (\(colIndex, cell) -> do
--             putStr (show cell ++ " ")
--             if colIndex `mod` 3 == 2 && colIndex /= numCols - 1
--                 then putStr "| "
--                 else return ()
--             ) (zip [0..] row)
--         putStrLn "|"

--ok
getCell :: [[[Int]]] -> (Int, Int, Int) -> Int
getCell board (x, y, z) = (board !! x) !! y !! z

neighbors :: [[[Int]]] -> [[String]] -> Int -> Int -> (Int, Int, Int, Int)
neighbors board sign_board row column = (top_num, right_num, bottom_num, left_num)
  where
    size = length board

    top = sign_board !! row !! column
    right = if column /= size - 1 then sign_board !! row !! (column + 1) else ""
    bottom = if row < size - 1 then sign_board !! (row + 1) !! column else ""
    left = if column > 0 then sign_board !! row !! (column - 1) else ""

    top_num =
      if row > 0
        then case top of
          "v" -> maximum (board !! (row - 1) !! column)
          "^" -> minimum (board !! (row - 1) !! column)
          _ -> 0
        else 0

    right_num =
      if column /= size - 1
        then case right of
          "<" -> maximum (board !! row !! (column + 1))
          ">" -> minimum (board !! row !! (column + 1))
          _ -> 0
        else 0

    bottom_num =
      if row < size - 1
        then case bottom of
          "^" -> maximum (board !! (row + 1) !! column)
          "v" -> minimum (board !! (row + 1) !! column)
          _ -> 0
        else 0

    left_num =
      if column > 0
        then case left of
          ">" -> maximum (board !! row !! (column - 1))
          "<" -> minimum (board !! row !! (column - 1))
          _ -> 0
        else 0


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
    printCountPossibilities board
    -- printMatrix board2
    -- putStrLn (show (isFull board2))
    -- putStrLn (show (isFull board))
    -- putStrLn (show (neighborsSigns sign_board 0 0))
    -- putStrLn (show (nextCell (0, 0) size))
    -- putStrLn (show (getCell board2 (0, 0, 0)))
    printSudoku board2
    putStrLn (show (neighbors board2 sign_board 0 0))

