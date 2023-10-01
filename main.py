from tests import board, size, region_size, sign_board

def next_cell(cell):
    return (cell[0], cell[1]+1) if cell[1] < size-1 else (cell[0]+1, 0)

def is_full(board):
    return all(len(x) == 1 for row in board for x in row)

def board_copy(board): return [[[x for x in cell] for cell in row] for row in board]

def neighbors_list(board, row, column): #retorna uma lista com os vizinhos
    top_num = board[row-1][column] if row > 0 else 0
    right_num = board[row][column+1] if column != (size-1) else 0
    bottom_num = board[row+1][column] if row < size-1 else 0
    left_num = board[row][column-1] if column != size else 0
    neighbors_list = [top_num, right_num, bottom_num, left_num]
    return neighbors_list

def neighbors(board, row, column):
    top, right, bottom, left = sign_board[row][column]
    top_num = right_num = bottom_num = left_num = 0

    if len(board[row][column]) > 1:
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

    else:
        if row > 0 and len(board[row-1][column]) == 1:
            top_num = board[row-1][column][0]
        if column != (size-1) and len(board[row][column+1]) == 1:
            right_num = board[row][column+1][0]
        if row < size-1 and len(board[row+1][column]) == 1:
            bottom_num = board[row+1][column][0]
        if column != size  and len(board[row][column-1]) == 1:
            left_num = board[row][column-1][0]

    return top_num, right_num, bottom_num, left_num

def neighbors_signs(board, row, column):
    top, right, bottom, left = sign_board[row][column]
    return top, right, bottom, left

def is_valid(board, size, region_size):
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

def is_placement_valid(board, row, column, value, size, region_size, 
                       top_num=0, right_num=0, bottom_num=0, left_num=0,
                       top='.', right='.', bottom='.', left='.'):
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
    if not sign_valid(top, right, bottom, left, value, top_num, right_num, bottom_num, left_num):
        return False    
    
    return True

def filter(board, number): #a cada celula marcada, filtra as outras possibilidades
    # Check rows
    # Check columns
    # Check blocks
    pass

def sign_valid(top, right, bottom, left, number, top_num, right_num, bottom_num, left_num):     
    if top != '.' and top_num != 0:
        if top == '^' and number <= top_num:
            return False
        elif top == 'v' and number >= top_num:
            return False
    if right != '.' and right_num != 0:
        if right == '<' and number >= right_num:
            return False
        elif right == '>' and number <= right_num:
            return False
    if bottom != '.' and bottom_num != 0:
        if bottom == '^' and number >= bottom_num:
            return False
        elif bottom == 'v' and number <= bottom_num:
            return False
    if left != '.' and left_num != 0:
        if left == '<' and number <= left_num:
            return False
        elif left == '>' and number >= left_num:
            return False
    return True

def print_board(board, size):
    print("--"*size)
    for i in range(len(board)):
        for j in range(len(board)):
            if len(cell:=board[i][j]) > 1:
                print('□', end=' ')
            else: print(cell[0], end=' ')
        print()
    print("--"*size)

def count_pops(top, right, bottom, left):
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

def pre_process(board, size, region_size):
    copy = [[[x for x in cell] for cell in row] for row in board]
    success = True
    total = 0
    
    # Remove borders for smaller or bigger neighbors
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

    while success:
        total += 1
        success = False
        for row in range(size):
            for column in range(size):
                if len(copy[row][column]) == 1:
                    continue
                symbols = [x for x in sign_board[row][column]]
                neighbors_numbers = neighbors(board, row, column)
                for k in copy[row][column]:
                    if not is_placement_valid(copy, row, column, k, size, region_size, *neighbors_numbers, *symbols):
                        copy[row][column].remove(k)
                        success = True
    print(total)
    return copy

def solve(board, row, column, size, region_size, checks=0, nivel=0):
    if is_full(board):
        if is_valid(board, size, region_size):
            return board, checks
        return False, checks
    
    # Next cell
    next_row, next_column = next_cell((row, column))
    
    # Neighbors value comparison
    top, right, bottom, left = sign_board[row][column]
    symbols = top, right, bottom, left
    
    neighbors_ = neighbors(board, row, column)

    if len(board[row][column]) == 0:
        return False, checks

    # Try possibilities
    for i in board[row][column]:
        copy = [[[x for x in cell] for cell in row] for row in board]
        copy[row][column] = [i]
        checks += 1
        if not is_placement_valid(copy, row, column, i, size, region_size, *neighbors_, *symbols):
            continue
        if (result := solve(copy, next_row, next_column, size, region_size, checks, nivel=nivel+1))[0] != False:
            return result
    return False, checks

def print_count_possibilities(board, size):
    print("--"*size)
    for i in range(len(board)):
        for j in range(len(board)):
            print(len(board[i][j]), end=' ')
        print()
    print("--"*size)
