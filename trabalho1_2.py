import random
#comparative sudoku or Vergleichssudoku

def solve_puzzle(board, output_board):
    for i in range(81):
        top, right, bottom, left = list(board[i])

        for j in range(len(output_board[i])):
            top_num, right_num, bottom_num, left_num = 0, 0, 0, 0
            top_num = output_board[i-9][j] if i-9 >-1 and i-9 < 81 else top_num
            right_num = output_board[i+1][j] if i+1 >-1 and i+1 < 81 else right_num
            bottom_num = output_board[i+9][j] if i+9 >-1 and i+9 < 81 else bottom_num
            left_num = output_board[i-1][j] if i-1 >-1 and i-1 < 81 else left_num
            for k in range (1,10):
                if is_valid(top, right, bottom, left, k, top_num, right_num, bottom_num, left_num):
                    if output_board[i] is not list:
                        output_board[i] = k
                    else:
                        output_board[i].append(k)
        for k in range (1,10):
            if is_valid(top, right, bottom, left, k, top_num, right_num, bottom_num, left_num, i):
                output_board[i].append(k)
        if output_board[i] == 0:
            print("deu problema em i : " + str(i))

def is_valid_column(number, output_board, col_num):
    aux = []
    for x in range (col_num, 81, 9):
        aux.append(output_board[x])
    if number in aux:
        return False
    return True

def is_valid_line(number, output_board, line_num):
    aux = []
    for x in range (line_num*9, line_num*9+9):
        aux.append(output_board[x])
    if number in aux:
        return False
    return True

def is_valid_region(number, output_board, reg_num):
    pass

def is_valid(top, right, bottom, left, number, top_num, right_num, bottom_num, left_num, i, output_board):
    if top != '.':
        if top == '^' and number <= top_num and top_num != 0:
            print('top1'+ str(number))
            return False
        elif top == 'v' and number >= top_num and top_num != 0:
            print('top2'+ str(number))
            return False
    if right != '.':
        if right == '<' and number >= right_num and right_num != 0:
            print('right1'+ str(number))
            return False
        elif right == '>' and number <= right_num and right_num != 0:
            print('right2'+ str(number))
            return False
    if bottom != '.':
        if bottom == '^' and number >= bottom_num and bottom_num != 0:
            print('bottom1'+ str(number))
            return False
        elif bottom == 'v' and number <= bottom_num and bottom_num != 0:
            print('bottom2'+ str(number))
            return False
    if left != '.':
        if left == '<' and number <= left_num and left_num != 0:
            print('left1'+ str(number))
            return False
        elif left == '>' and number >= left_num and left_num != 0:
            print('left2'+ str(number))
            return False
    if not is_valid_column(number, output_board, i%9):
        return False
    if not is_valid_line(number, output_board, i/9):
        return False
    if not is_valid_region(number, output_board, i):
        return False
    return True

def is_solved(output_board):
    #check rows
    aux = 0
    for i in range(9):
        for j in range(9):
            aux += output_board[i*9+j]
        if aux != 45:    
            return False
    #check columns
    aux = 0
    for i in range(9):
        for j in range(9):
            aux += output_board[i+j*9]
        if aux != 45:    
            return False
    #check areas
    for k in range (9):
        aux = 0
        for i in range(3):
            for j in range(3):
                aux += output_board[(i*3+k)*9+j] ###############
        if aux != 45:    
            return False
    return True

def main():

    board = [ #top, right, bottom, left
        '.>^.', '.<v>', '.<^.',      '.<^.', '.<v>', '..v<',      '.>^.', '.<^>', '..^<',
        'v>^.', 'v<v>', 'v.v<',      '^>^.', 'v<^>', 'v.v<',      '^>v.', '^<^>', '^.v<',
        '^>..', 'v<.>', 'v..<',      '^>..', '^>.>', 'v..>',      'v>..', '^<.>', 'v..<',

        '.<v.', '.>v<', '..v>',      '.<^.', '.<^<', '..^<',      '.<^.', '.<^<', '..v<',
        'v<^.', 'v>^<', 'v.v>',      '^<v.', '^<v<', '^.v<',      '^<^.', '^>v<', 'v.^>',
        '^<..', '^>.<', 'v..>',      'v<..', 'v>.<', 'v..>',      '^<..', 'v<.<', '^..<',

        '.>v.', '.>v>', '..^>',      '.>^.', '.<^>', '..^<',      '.>v.', '.<v>', '..v<',
        'v>^.', 'v<^>', '^.v<',      '^<^.', '^>v<', '^.v>',      '.>v.', 'v>^<', 'v.^>',
        '^<..', '^>.<', 'v..>',      '^>..', 'v>.>', 'v..>',      'v<..', '^>.<', '^..>',
    ]



    # output_board = [[8],[0],[0],[2],[0],[6],[0],[0],[4],
    #                 [0],[3],[0],[0],[0],[0],[0],[2],[0],
    #                 [0],[0],[0],[0],[8],[0],[0],[0],[0],
    #                 [2],[0],[0],[0],[0],[0],[0],[0],[6],
    #                 [0],[0],[5],[0],[0],[0],[2],[0],[0],
    #                 [4],[0],[0],[0],[0],[0],[0],[0],[9],
    #                 [0],[0],[0],[0],[2],[0],[0],[0],[0],
    #                 [0],[2],[0],[0],[0],[0],[0],[5],[0],
    #                 [5],[0],[0],[8],[0],[1],[0],[0],[2],
    # ]

    output_board = [[x for x in range(1, 10)] * 81]

    output_board[0]  = [8]
    output_board[3]  = [2]
    output_board[5]  = [6]
    output_board[8]  = [4]
    output_board[10] = [3]
    output_board[16] = [2]
    output_board[22] = [8]
    output_board[27] = [2]
    output_board[35] = [6]
    output_board[38] = [5]
    output_board[42] = [2]
    output_board[45] = [4]
    output_board[53] = [9]
    output_board[58] = [2]
    output_board[64] = [2]
    output_board[70] = [5]
    output_board[72] = [8]
    output_board[75] = [8]
    output_board[77] = [1]
    output_board[80] = [2]
    
    solve_puzzle(board, output_board)
    if (is_solved(output_board)):
        print('is solved\n{}'.format(output_board))
    # else:
    #     for _ in range (1000):
    #         solve_puzzle(board, output_board, True)
    #         if is_solved(output_board):
    #             print('is solved\n{}'.format(output_board))
    #             break
    #     print(output_board)
        #print('no solution\n{}'.format(output_board))

if __name__ == "__main__":
    main()


