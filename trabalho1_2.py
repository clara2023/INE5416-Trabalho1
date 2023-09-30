from tests import board, size, region_size, sign_board

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
    # Check blocks
    for i in range(0, size, region_size):
        for j in range(0, size, region_size):
            regiao = [board[x][y] for x in range(i, i+region_size) for y in range(j, j+region_size)]
            regiao.sort()
            for k in range(size-1):
                if regiao[k] == regiao[k+1]:
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
    print('number: ', value)
    print(top_num, right_num, bottom_num, left_num)
    if not sign_valid(top, right, bottom, left, value, top_num, right_num, bottom_num, left_num):
        print("sign")
        return False    
    return True

def sign_valid(top, right, bottom, left, number, top_num, right_num, bottom_num, left_num):     
    if top != '.':
        if top == '^' and number <= top_num and top_num != 0:
            print("top", number, top_num)
            return False
        elif top == 'v' and number >= top_num and top_num != 0:
            print("top", number, top_num)
            return False
    if right != '.':
        if right == '<' and number >= right_num and right_num != 0:
            print("right", number, right_num)   
            return False
        elif right == '>' and number <= right_num and right_num != 0:
            print("right", number, right_num)
            return False
    if bottom != '.':
        if bottom == '^' and number >= bottom_num and bottom_num != 0:
            print("bottom", number, bottom_num)
            return False
        elif bottom == 'v' and number <= bottom_num and bottom_num != 0:
            print("bottom", number, bottom_num)
            return False
    if left != '.':
        if left == '<' and number <= left_num and left_num != 0:
            print("left", number, left_num)
            return False
        elif left == '>' and number >= left_num and left_num != 0:
            print("left", number, left_num)
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
                        top_num = right_num = bottom_num = left_num = 0
                        top = sign_board[i][j][0]
                        right = sign_board[i][j][1]
                        bottom = sign_board[i][j][2]
                        left = sign_board[i][j][3]
                        #filtra o valor se houver uma casa marcada 
                        if len(board[i-1][j]) == 1 and i > 0 and sign_board[i-1][j][0] != '.':
                            top_num = board[i-1][j]
                        if j != (size-1) and len(board[i][j+1]) == 1 and sign_board[i][j+1][1] != '.':
                            right_num = board[i][j+1]
                        if i < size-1 and len(board[i+1][j]) == 1 and sign_board[i+1][j][2] != '.':
                            bottom_num = board[i+1][j]
                        if j != size  and len(board[i][j-1]) == 1 and sign_board[i][j-1][3] != '.':
                            left_num = board[i][j-1]
                        if not is_placement_valid(copy, i, j, k, size, region_size, top_num, right_num, bottom_num, left_num, top, right, bottom, left):
                            copy[i][j].remove(k)
                            success = True
        return copy
    
def solve(board, row, column, size, region_size, checks=0):
    if is_full(board):
        if is_valid(board, size, region_size):
            return board, checks
        return False, checks
    
    next_row, next_column = next_cell((row, column))
    top_num = right_num = bottom_num = left_num = 0
    top = sign_board[row][column][0]
    right = sign_board[row][column][1]
    bottom = sign_board[row][column][2]
    left = sign_board[row][column][3]

    if len(board[row-1][column]) == 1 and row > 0:
        top_num = board[row-1][column]
    # if len(board[row][column+1]) == 1 and (row*size + column)%(size-1) != 0 and sign_board[row][column+1][1] != '.':
    if column != (size-1) and len(board[row][column+1]) == 1 and sign_board[row][column+1][1] != '.':
        right_num = board[row][column+1]
    if row < size-1 and len(board[row+1][column]) == 1 and sign_board[row+1][column][2] != '.':
        bottom_num = board[row+1][column]
    if column != size  and len(board[row][column-1]) == 1 and sign_board[row][column-1][3] != '.':
        left_num = board[row][column-1]

    for i in board[row][column]:
        copy = [[x for x in y] for y in board]
        copy[row][column] = [i]
        checks += 1
        if not is_placement_valid(copy, row, column, i, size, region_size, top_num, right_num, bottom_num, left_num, top, right, bottom, left):
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
