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

-- funcao auxiliar, talvez seja utilizada
-- split :: Char -> String -> [String]
-- split delimiter input = split' input []
--   where
--     split' "" acc = reverse acc
--     split' str acc =
--       let (before, after) = span (/= delimiter) str
--           rest = dropWhile (== delimiter) after
--       in split' rest (before : acc)

isPlacementValid :: [[[Int]]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Char -> Char -> Char -> Char -> Bool
isPlacementValid board row column value size regionSize topNum rightNum bottomNum leftNum top right bottom left =
    let isRowValid = all (\(i, element) -> length element /= 1 || (element !! 0) /= value || i == column) (zip [0..] (board !! row))
        isColumnValid = all (\i -> length (board !! i !! column) /= 1 || (board !! i !! column !! 0) /= value || i == row) [0..size-1]
        regionRow = row `div` regionSize
        regionColumn = column `div` regionSize
        isRegionValid = all (\i -> all (\j -> length (board !! i !! j) /= 1 || (board !! i !! j !! 0) /= value || (i, j) == (row, column)) [regionColumn*regionSize..(regionColumn+1)*regionSize-1]) [regionRow*regionSize..(regionRow+1)*regionSize-1]
        areSignsValid = signValid top right bottom left value topNum rightNum bottomNum leftNum
    in isRowValid && isColumnValid && isRegionValid && areSignsValid


--imprime traços
printCountPossibilities2 :: Int -> IO()
printCountPossibilities2 size = do
    putStrLn (replicate (2 * size + 2) '-')

--imprime matriz de possibilidades
printMatrix :: [[[Int]]] -> IO ()
printMatrix matrix = mapM_ printLayer matrix
  where
    printLayer layer = do
        printList layer
        putStrLn ""

    printList list = do
        mapM_ printCell list
        
    printCell cell = do 
        putStr(show (length cell))

printCountPossibilities :: Int -> [[[Int]]] -> IO()
printCountPossibilities size board = do 
    printCountPossibilities2 size
    printMatrix board
    printCountPossibilities2 size

make_board :: Int -> [[[Int]]]
make_board size = replicate size $ replicate size [1..size]

main :: IO ()
main = do
    let size = 9
        board =  make_board size
    printCountPossibilities size board
