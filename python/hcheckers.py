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

app = QApplication(sys.argv)

window = Checkers(locate_share_dir())
window.show()

sys.exit(app.exec_())

