#!/usr/bin/python3

import sys
import os
from os.path import join, exists, dirname, abspath
import gettext
import logging

bindir = abspath(dirname(sys.argv[0]))
if exists(join(bindir, "themes")):
    print("Development mode")
    sys.path.insert(0, dirname(bindir))

import hcheckers

def locate_share_dir():
    home = os.environ["HOME"]
    moduledir = abspath(dirname(hcheckers.__file__))
    bases = ["/usr/share/hcheckers", "/usr/local/share/hcheckers",
             join(home, ".local", "share", "hcheckers"),
             moduledir, bindir]
    for base in bases:
        themes = join(base, "themes")
        if exists(base) and exists(themes):
            return base
    raise Exception("Cannot locate share directory; checked were: {}".format(bases))

def locate_locales():
    share = locate_share_dir()
    locales = join(share, "locale")
    logging.info("Using locales at: {}".format(locales))
    return locales

if sys.version_info[0] == 2:
    gettext.install("hcheckers", localedir=locate_locales(), unicode=True)
else:
    gettext.install("hcheckers", localedir=locate_locales())

from PyQt5.QtWidgets import QApplication, QWidget
from PyQt5.QtCore import Qt, QCoreApplication

from hcheckers.toplevel import Checkers

QCoreApplication.setAttribute(Qt.AA_EnableHighDpiScaling)
app = QApplication(sys.argv)

window = Checkers(locate_share_dir())
window.show()

sys.exit(app.exec_())

