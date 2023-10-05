from tests import board, sign_board
from main import *

def uuh(board, sign_board):
    solved = solve(board, sign_board)
    if solved != False:
        print_board(solved)
    else:
        print("No solutions found!")

print_board(board)
print("Preprocessing...")
board = vergleisch_preprocess(board, sign_board)
board = pre_process(board, sign_board)
print_board(board)
print_count_possibilities(board)

uuh(board, sign_board)