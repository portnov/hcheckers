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
checker["m2"] = {'kind': 'Man', 'side': 'Second'}

board = [['c3', checker["m1"]],
         ['e3', checker["m1"]],
         ['e5', checker["m2"]],
         ['f6', checker["m2"]]
        ]

theme = Theme("themes/default", None)
game = Game()
ai = AI(depth=6, board=None, load=True, store=True, update_cache_max_depth=8, update_cache_max_pieces=16)
game.start_new_game("portnov", user_turn_first=False,  ai=ai)

app = QApplication(sys.argv)

board = Board(theme, game)
board.field_clicked.connect(clicked)
board.show()

sys.exit(app.exec_())

