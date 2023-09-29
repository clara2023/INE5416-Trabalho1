from tests import board, size, region_size

def next_cell(cell):
    return (cell[0], cell[1]+1) if cell[1] < size-1 else (cell[0]+1, 0)

def is_full(board):
    return all(len(x) == 1 for row in board for x in row)

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
    # Check blocks later
    for i in range(0, size, region_size):
        for j in range(0, size, region_size):
            regiao = [board[x][y] for x in range(i, i+region_size) for y in range(j, j+region_size)]

            regiao.sort()
            for k in range(size-1):
                if regiao[k] == regiao[k+1]:
                    return False
        return True    

def is_placement_valid(board, row, column, value, size, region_size):
    # Check row
    for i, element in enumerate(board[row]):
        if len(element) == 1 and element[0] == value and i != column:
            return False
    # Check column
    for i in range(size):
        if len(board[i][column]) == 1 and board[i][column][0] == value and i != row:
            return False
    # Check blocks later
    region_row = row//region_size
    region_column = column//region_size
    for i in range(region_row*region_size, (region_row+1)*region_size):
        for j in range(region_column*region_size, (region_column+1)*region_size):
            if len(board[i][j]) == 1 and board[i][j][0] == value and (i, j) != (row, column):
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

def pre_process(board, size, region_size):
        copy = [[x for x in y] for y in board]
        success = True
        total = 0
        while success:
            total += 1
            success = False
            for i in range(size):
                for j in range(size):
                    if len(copy[i][j]) == 1:
                        continue
                    for k in copy[i][j]:
                        if not is_placement_valid(copy, i, j, k, size, region_size):
                            copy[i][j].remove(k)
                            success = True
        return copy
    
def solve(board, row, column, size, region_size, checks=0):
    if is_full(board):
        if is_valid(board, size, region_size):
            return board, checks
        return False, checks
    
    next_row, next_column = next_cell((row, column))

    for i in board[row][column]:
        copy = [[x for x in y] for y in board]
        copy[row][column] = [i]
        checks += 1
        if not is_placement_valid(copy, row, column, i, size, region_size):
            continue
        if (result := solve(copy, next_row, next_column, size, region_size, checks))[0] != False:
            return result
    return False, checks

def print_count_possibilities(board, size):
    print("--"*size)
    for i in range(len(board)):
        for j in range(len(board)):
            print(len(board[i][j]), end=' ')
        print()
    print("--"*size)

new_board, checks = solve(board, 0, 0, size, region_size)
if new_board != False:
    print_board(new_board, size)
else:
    print("No solution found.")

print("Checks: ", checks)