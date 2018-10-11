#!/usr/bin/python

import sys
from PyQt5.QtWidgets import QApplication, QWidget

from board import Board
from game import Game
from theme import Theme

def clicked(row, col):
    print("Clicked: {} {}".format(row, col))

theme = Theme("themes/default", None)
game = Game()
game.start_new_game("portnov")

app = QApplication(sys.argv)

board = Board(theme, game)
board.field_clicked.connect(clicked)
board.show()

sys.exit(app.exec_())

