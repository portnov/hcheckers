#!/usr/bin/python

import sys
from PyQt5.QtWidgets import QApplication, QWidget

from board import Board
from game import Game, AI
from theme import Theme
from toplevel import Checkers

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

app = QApplication(sys.argv)

window = Checkers()
window.show()

sys.exit(app.exec_())

