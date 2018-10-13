#!/usr/bin/python

import sys
from PyQt5.QtWidgets import QApplication, QWidget

from board import Board
from game import Game
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
game.start_new_game("portnov", ai_depth=8, board=None, user_turn_first=True)

app = QApplication(sys.argv)

board = Board(theme, game)
board.field_clicked.connect(clicked)
board.show()

sys.exit(app.exec_())

