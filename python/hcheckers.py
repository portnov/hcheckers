#!/usr/bin/python

import sys
from PyQt5.QtWidgets import QApplication, QWidget

from board import Board
from game import Game, AI
from theme import Theme

def clicked(row, col):
    print("Clicked: {} {}".format(row, col))

checker = {}
checker["m1"] = {'kind': 'Man', 'side': 'First'}
checker["k1"] = {'kind': 'King', 'side': 'First'}
checker["m2"] = {'kind': 'Man', 'side': 'Second'}
checker["k2"] = {'kind': 'King', 'side': 'Second'}

board = [['b2', checker["m2"]],
         ['d4', checker["m2"]],
         ['d6', checker["m2"]],
         ['f4', checker["m2"]],
         ['c3', checker["k1"]]
        ]

theme = Theme("themes/default", None)
game = Game()
ai = AI(depth=8, board=None, load=True, store=True, update_cache_max_depth=8, update_cache_max_pieces=16)
game.start_new_game("portnov", user_turn_first=False,  ai=ai)

app = QApplication(sys.argv)

board = Board(theme, game)
board.field_clicked.connect(clicked)
board.show()

sys.exit(app.exec_())

