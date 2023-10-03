from main import *

def uuh(boa):
    solved, checks = solve(boa, 0, 0, size, region_size)
    if solved != False:
        print_board(solved, size)
    else:
        print("No solutions found!")

print_board(board, size)
print("Preprocessing...")
board = pre_process(board, size, region_size)
print_board(board, size)