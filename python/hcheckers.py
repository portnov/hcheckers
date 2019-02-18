#!/usr/bin/python

import sys
import os
from os.path import join, exists, dirname, abspath
import gettext
import logging

def locate_share_dir():
    home = os.environ["HOME"]
    bindir = abspath(dirname(sys.argv[0]))
    logging.info(bindir)
    bases = ["/usr/share/hcheckers", "/usr/local/share/hcheckers",
             join(home, ".local", "share", "hcheckers"),
             bindir]
    for base in bases:
        themes = join(base, "themes")
        if exists(base) and exists(themes):
            return base
    return None

def locate_locales():
    share = locate_share_dir()
    if share is None:
        raise Exception("Cannot locate share directory")
    locales = join(share, "locale")
    logging.info("Using locales at: {}".format(locales))
    return locales

if sys.version_info[0] == 2:
    gettext.install("hcheckers", localedir=locate_locales(), unicode=True)
else:
    gettext.install("hcheckers", localedir=locate_locales())

from PyQt5.QtWidgets import QApplication, QWidget

import newgamedlg
import settingsdlg
from board import Board
from game import Game, AI
from theme import Theme
from toplevel import Checkers

def clicked(row, col):
    logging.debug("Clicked: {} {}".format(row, col))

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

window = Checkers(locate_share_dir())
window.show()

sys.exit(app.exec_())

