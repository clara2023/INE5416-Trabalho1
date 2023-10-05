def next_cell(cell: tuple[int, int], size: int) -> tuple[int, int]:
    return (cell[0], cell[1]+1) if cell[1] < size-1 else (cell[0]+1, 0)

def is_full(board: list[list[list[int]]]) -> bool:
    return all(len(x) == 1 for row in board for x in row)

def board_copy(board: list[list[list[int]]]) -> list[list[list[int]]]:
    return [[[x for x in cell] for cell in row] for row in board]

def neighbors(board: list[list[list[int]]], sign_board: list[list[str]], row: int, column: int) -> tuple[int, int, int, int]:
    size = len(board)
    top, right, bottom, left = sign_board[row][column]
    top_num = right_num = bottom_num = left_num = 0

    if row > 0:
        if top == "v":
            top_num = max(board[row-1][column])
        elif top == "^":
            top_num = min(board[row-1][column])
    if column != (size-1):
        if right == "<":
            right_num = max(board[row][column+1])
        elif right == ">":
            right_num = min(board[row][column+1])
    if row < size-1:
        if bottom == "^":
            bottom_num = max(board[row+1][column])
        elif bottom == "v":
            bottom_num = min(board[row+1][column])
    if column != size:
        if left == ">":
            left_num = max(board[row][column-1])
        elif left == "<":
            left_num = min(board[row][column-1])

    return top_num, right_num, bottom_num, left_num

def neighbors_signs(sign_board: list[list[str]], row: int, column: int) -> tuple[str, str, str, str]:
    top, right, bottom, left = sign_board[row][column]
    return top, right, bottom, left

def is_valid(board: list[list[list[int]]]) -> bool:
    size = len(board)
    region_size = int(size**0.5)
    # If board is not full, invalid
    if not is_full(board):
        return False
    # Check rows
    for i in range(size):
        row = [x for x in board[i]]
        row.sort()
        for j in range(size-1):
            if row[j] == row[j+1]:
                return False
    # Check columns
    for i in range(size):
        column = []
        for j in range(size):
            column.append(board[j][i])
        column.sort()
        for j in range(size-1):
            if column[j] == column[j+1]:
                return False
    # Check blocks
    for i in range(0, size, region_size):
        for j in range(0, size, region_size):
            region = [board[x][y] for x in range(i, i+region_size) for y in range(j, j+region_size)]
            region.sort()
            for k in range(size-1):
                if region[k] == region[k+1]:
                    return False
    return True    

def is_placement_valid(board: list[list[list[int]]], sign_board: list[list[str]], row: int, column: int, value: int) -> bool:
    size = len(board)
    region_size = int(size**0.5)
    # Check row
    for i, element in enumerate(board[row]):
        if len(element) == 1 and element[0] == value and i != column:
            return False
    # Check column
    for i in range(size):
        if len(board[i][column]) == 1 and board[i][column][0] == value and i != row:
            return False
    # Check blocks
    region_row = row//region_size
    region_column = column//region_size
    for i in range(region_row*region_size, (region_row+1)*region_size):
        for j in range(region_column*region_size, (region_column+1)*region_size):
            if len(board[i][j]) == 1 and board[i][j][0] == value and (i, j) != (row, column):
                return False
    # Check signs
    symbols = [x for x in sign_board[row][column]]
    neighbors_numbers = neighbors(board, sign_board, row, column)
    if not sign_valid(value, *symbols, *neighbors_numbers):
        return False    
    
    return True

def sign_valid(number: int, top: str, right: str, bottom: str, left: str, top_num: int, right_num: int, bottom_num: int, left_num: int) -> bool:
    if top == '^' and top_num != 0 and number <= top_num:
        return False
    if top == 'v' and top_num != 0 and number >= top_num:
        return False

    if right == '>' and right_num != 0 and number <= right_num:
        return False
    if right == '<' and right_num != 0 and number >= right_num:
        return False

    if bottom == 'v' and bottom_num != 0 and number <= bottom_num:
        return False
    if bottom == '^' and bottom_num != 0 and number >= bottom_num:
        return False

    if left == '<' and left_num != 0 and number <= left_num:
        return False
    if left == '>' and left_num != 0 and number >= left_num:
        return False

    return True

def print_board(board: list[list[list[int]]]) -> None:
    size = len(board)
    region_size = int(size**0.5)
    print("-"*(2*size+2))
    for i in range(len(board)):
        for j in range(len(board)):
            if len(cell:=board[i][j]) > 1:
                print('□', end=' ')
            else: print(cell[0], end=' ')
            if (j+1)%region_size == 0:
                print(' ', end='')
        print()
        if (i+1)%region_size == 0:
            print()
    print("-"*(2*size+2))

def count_pops(top: str, right: str, bottom: str, left: str) -> tuple[int, int]:
    pop_front = 0
    pop_back = 0

    pop_front += top == '^'
    pop_front += right == '>'
    pop_front += bottom == 'v'
    pop_front += left == '<'

    pop_back += top == 'v'
    pop_back += right == '<'
    pop_back += bottom == '^'
    pop_back += left == '>'

    return pop_front, pop_back

def vergleisch_preprocess(board: list[list[list[int]]], sign_board: list[list[str]]) -> list[list[list[int]]]:
    size = len(board)
    copy = board_copy(board)
    for row in range(size):
        for column in range(size):
            if len(copy[row][column]) != size:
                continue
            top, right, bottom, left = sign_board[row][column]
            pop_front, pop_back = count_pops(top, right, bottom, left)
            for _ in range(pop_front):
                copy[row][column].pop(0)
            for _ in range(pop_back):
                copy[row][column].pop()
    return copy

def pre_process(board: list[list[list[int]]], sign_board: list[list[str]]) -> list[list[list[int]]]:
    size = len(board)
    copy = board_copy(board)
    success = True
    total = 0

    while success:
        total += 1
        success = False
        for row in range(size):
            for column in range(size):
                if len(copy[row][column]) == 1:
                    continue
                for k in copy[row][column]:
                    if not is_placement_valid(copy, sign_board, row, column, k):
                        copy[row][column].remove(k)
                        success = True
    return copy

def solve(board: list[list[list[int]]], sign_board: list[list[str]], row: int=0, column: int=0) -> bool | list[list[list[int]]]:
    if is_full(board):
        if is_valid(board):
            return board
        return False
    
    # Next cell
    next_row, next_column = next_cell((row, column), len(board))

    if len(board[row][column]) == 0:
        return False

    # Try possibilities
    for i in board[row][column]:
        copy = pre_process(board, sign_board)
        copy[row][column] = [i]
        if not is_placement_valid(copy, sign_board, row, column, i):
            continue
        result = solve(copy, sign_board, next_row, next_column)
        if result != False:
            return result
    return False

def print_count_possibilities(board: list[list[list[int]]]) -> None:
    size = len(board)
    region_size = int(size**0.5)

    print("-"*(2*size+2))
    for i in range(len(board)):
        for j in range(len(board)):
            print(len(board[i][j]), end=' ')
            if (j+1)%region_size == 0:
                print(' ', end='')
        print()
        if (i+1)%region_size == 0:
            print()
    print("-"*(2*size+2))
