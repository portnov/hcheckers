#!/usr/bin/python

import sys
from PyQt5.QtWidgets import QApplication, QWidget

from board import Board
from game import Game
from theme import Theme

def clicked(row, col):
    print("Clicked: {} {}".format(row, col))

board = [['h6', {'kind': 'Man', 'side': 'First'}],
         ['g7', {'kind': 'Man', 'side': 'Second'}]
        ]

theme = Theme("themes/default", None)
game = Game()
game.start_new_game("portnov", ai_depth=4, board=None)

app = QApplication(sys.argv)

board = Board(theme, game)
board.field_clicked.connect(clicked)
board.show()

sys.exit(app.exec_())

