
from os.path import join, exists
import os

from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QApplication, QWidget, QToolBar, QMainWindow, QDialog, QVBoxLayout, QAction, QActionGroup, QLabel

from field import Field
from game import Game, AI, RequestError
from board import Board
from theme import Theme
from newgamedlg import *

def locate_share_dir():
    home = os.environ["HOME"]
    bases = ["/usr/share/hcheckers", "/usr/local/share/hcheckers",
             join(home, ".local", "share", "hcheckers"),
             "."]
    for base in bases:
        themes = join(base, "themes")
        if exists(base) and exists(themes):
            return base
    return None

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
        self.share_dir = locate_share_dir()
        if self.share_dir is None:
            raise Exception("Cant locate share directory")
        theme_name = self.settings.value("theme", "default")
        self.theme = Theme(join(self.share_dir, "themes", theme_name), None)
        self.game = Game()
        self.poll_timer = self.startTimer(500)
        self.do_poll = False

    def _gui_setup(self):
        widget = QWidget(self)
        layout = QVBoxLayout()
        self.board = Board(self.theme, self.game)
        self.board.message.connect(self._on_board_message)
        #self.board.show()
        self.toolbar = QToolBar(self)
        self.message = QLabel(self)
        layout.addWidget(self.toolbar)
        layout.addWidget(self.message)
        layout.addWidget(self.board, stretch=1)
        widget.setLayout(layout)
        self.setCentralWidget(widget)
    
    def _create_action(self, icon, title, menu, handler, group=None, toggle=False, toolbar=True, key=None):
        if group is None:
            parent = self
        else:
            parent = group
        action = QAction(title, parent)
        if key is not None:
            action.setShortcut(key)
        if toggle:
            action.setCheckable(True)
        if toolbar:
            self.toolbar.addAction(action)
        menu.addAction(action)
        if handler:
            action.triggered.connect(handler)
        return action

    def _setup_actions(self):
        menu = self.menuBar().addMenu("&Game")
        self._create_action(None, "New Game", menu, self._on_new_game, key="Ctrl+N")
        self._create_action(None, "Undo", menu, self._on_undo, key="Ctrl+Z")

        menu = self.menuBar().addMenu("&View")
        self._create_action(None, "Show notation", menu, self._on_toggle_notation, toolbar=False, toggle=True)

        menu.addSeparator()
        themes = QActionGroup(self)
        themes.setExclusive(True)
        self.themes = dict()
        for theme in Theme.list_themes(self.share_dir):
            self.themes[theme.name] = theme
            action = QAction(theme.name, self)
            action.setData(theme.name)
            action.setCheckable(True)
            if theme.name == "default":
                actin.setChecked(True)
            themes.addAction(action)
            menu.addAction(action)
        themes.triggered.connect(self._on_set_theme)

    def _default_new_game(self):
        self._on_new_game()

    def _on_new_game(self):
        dialog = NewGameDialog(self)
        result = dialog.exec_()
        if result == QDialog.Accepted:
            game = dialog.get_settings()
            if game.action == START_AI_GAME:
                self.game.start_new_game(game.user_name, user_turn_first=game.user_turn_first, ai=game.ai)
            elif game.action == START_HUMAN_GAME:
                game_id = self.game.new_game(game.rules)
                print(game_id)
                if game.user_turn_first:
                    self.game.register_user(game.user_name, FIRST)
                else:
                    self.game.register_user(game.user_name, SECOND)
                self.do_poll = True
            elif game.action == JOIN_HUMAN_GAME:
                self.game.game_id = dialog.lobby.get_game_id()
                self.game.user_side = side = dialog.lobby.get_free_side()
                self.game.register_user(game.user_name, side)
                self.game.run_game()
                self.do_poll = True
                self.board.my_turn = side == FIRST

            self.board.fields_setup()
            self.board.repaint()

    def _on_undo(self):
        prev_board = self.game.undo()
        self.board.fields_setup(prev_board)
        self.board.repaint()

    def _on_toggle_notation(self):
        self.board.show_notation = not self.board.show_notation

    def _on_set_theme(self, action):
        theme_name = action.data()
        theme = self.themes[theme_name]
        self.board.theme = theme

    def _on_board_message(self, message):
        self.message.setText(message)
        print(message)

    def timerEvent(self, e):
        if e.timerId() != self.poll_timer:
            return
        if not self.do_poll:
            return
        if self.board.my_turn:
            return

        board, messages = self.game.poll()
        for message in messages:
            self.board.process_message(message)
            self.board.my_turn = True

        self.board.fields_setup(board)

