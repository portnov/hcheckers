
from os.path import join

from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QApplication, QWidget, QToolBar, QMainWindow, QVBoxLayout, QAction

from field import Field
from game import Game, AI, RequestError
from board import Board
from theme import Theme

class Checkers(QMainWindow):
    def __init__(self):
        QMainWindow.__init__(self)
        self.setWindowTitle("HCheckers client")
        self.settings = QSettings("hcheckers", "hcheckers")
        self._prepare()
        self._gui_setup()
        self._setup_actions()
        self._default_new_game()

    def _prepare(self):
        theme_name = self.settings.value("theme", "default")
        self.theme = Theme(join("themes", theme_name), None)
        self.game = Game()

    def _gui_setup(self):
        widget = QWidget(self)
        layout = QVBoxLayout()
        self.board = Board(self.theme, self.game)
        #self.board.show()
        self.toolbar = QToolBar(self)
        layout.addWidget(self.toolbar)
        layout.addWidget(self.board)
        widget.setLayout(layout)
        self.setCentralWidget(widget)
    
    def _create_action(self, icon, title, menu, handler, key=None):
        action = QAction(title, self)
        if key is not None:
            action.setShortcut(key)
        self.toolbar.addAction(action)
        menu.addAction(action)
        if handler:
            action.triggered.connect(handler)
        return action

    def _setup_actions(self):
        menu = self.menuBar().addMenu("Game")
        self._create_action(None, "New Game", menu, self._on_new_game, key="Ctrl+N")
        self._create_action(None, "Undo", menu, self._on_undo, key="Ctrl+Z")

    def _default_new_game(self):
        ai = AI(depth=6, board=None, load=False, store=False, update_cache_max_depth=8, update_cache_max_pieces=16)
        self.game.start_new_game("portnov", user_turn_first=True, ai=ai)
        self.board.fields_setup()
        self.board.repaint()

    def _on_new_game(self):
        ai = AI(depth=6, board=None, load=False, store=False, update_cache_max_depth=8, update_cache_max_pieces=16)
        self.game.start_new_game("portnov", user_turn_first=True, ai=ai)
        self.board.fields_setup()
        self.board.repaint()

    def _on_undo(self):
        prev_board = self.game.undo()
        self.board.fields_setup(prev_board)
        self.board.repaint()

