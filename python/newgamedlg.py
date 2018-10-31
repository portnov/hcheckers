
import getpass

from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QWidget, QDialog, QPushButton, QVBoxLayout, QHBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, QDialogButtonBox, QFileDialog

from common import *
from game import AI, GameSettings
from lobby import LobbyWidget

START_AI_GAME = 1
START_HUMAN_GAME = 2
JOIN_HUMAN_GAME = 3

DEFAULT_BOARD = 1
MANUAL_BOARD = 2
LOAD_FEN = 3

class FileSelectWidget(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        layout = QHBoxLayout()
        self.textbox = QLineEdit(self)
        self.button = QPushButton("Browse...")
        self.button.clicked.connect(self._on_browse)
        layout.addWidget(self.textbox)
        layout.addWidget(self.button)
        self.setLayout(layout)

    def _on_browse(self):
        (path,_) = QFileDialog.getOpenFileName(self, "Open FEN file", ".", "FEN notation (*.fen)")
        self.textbox.setText(path)

    def path(self):
        return self.textbox.text()

class NewGameDialog(QDialog):
    def __init__(self, parent=None):
        QDialog.__init__(self, parent)

        widget = QWidget()
        layout = QFormLayout()

        self.rules = QComboBox()
        for (title, name) in supported_rules:
            self.rules.addItem(title, name)
        layout.addRow("Rules", self.rules)

        self.user_name = QLineEdit(self)
        self.user_name.setText(getpass.getuser())
        layout.addRow("User name", self.user_name)

        self.user_side = QComboBox(self)
        self.user_side.addItem("White", FIRST)
        self.user_side.addItem("Black", SECOND)
        layout.addRow("User plays", self.user_side)

        self.game_type = QComboBox(self)
        self.game_type.addItem("Start a game against computer", START_AI_GAME)
        self.game_type.addItem("Start a game against human", START_HUMAN_GAME)
        self.game_type.addItem("Join a game against human", JOIN_HUMAN_GAME)
        layout.addRow("Action", self.game_type)

        self.board_type = QComboBox(self)
        self.board_type.addItem("Use default initial position", DEFAULT_BOARD)
        self.board_type.addItem("Manual initial position setup", MANUAL_BOARD)
        self.board_type.addItem("Load initial board from FEN file", LOAD_FEN)
        layout.addRow("Initial board type", self.board_type)

        self.fen_path = FileSelectWidget(self)
        self.fen_path.setVisible(False)
        layout.addRow("Select FEN file", self.fen_path)

        widget.setLayout(layout)

        vbox = QVBoxLayout()
        vbox.addWidget(widget)

        self.ai_group = QGroupBox()
        self.ai_group.setTitle("AI Settings")
        ai_layout = QFormLayout()

        self.ai_depth = QComboBox(self)
        self.ai_depth.addItem("Level 2", 2)
        self.ai_depth.addItem("Level 4", 4)
        self.ai_depth.addItem("Level 6", 6)
        self.ai_depth.addItem("Level 8", 8)
        self.ai_depth.addItem("Level 9", 9)
        ai_layout.addRow("Strength", self.ai_depth)

        self.ai_load = QCheckBox(self)
        self.ai_load.setTristate(False)
        ai_layout.addRow("Load cache", self.ai_load)

        self.ai_save = QCheckBox(self)
        self.ai_save.setTristate(False)
        ai_layout.addRow("Save cache", self.ai_save)

        self.ai_group.setLayout(ai_layout)
        vbox.addWidget(self.ai_group)

        self.lobby = LobbyWidget(parent=self)
        self.lobby.hide()
        vbox.addWidget(self.lobby)

        self.setLayout(vbox)

        self.game_type.currentIndexChanged.connect(self._on_action_changed)
        self.board_type.currentIndexChanged.connect(self._on_board_type_changed)

        buttons = QDialogButtonBox(
                    QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
                    Qt.Horizontal, self)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        vbox.addWidget(buttons)

    def _on_action_changed(self, idx):
        action = self.game_type.itemData(idx)

        show_ai = action == START_AI_GAME
        show_lobby = action == JOIN_HUMAN_GAME
        show_side = action != JOIN_HUMAN_GAME
        show_board_setup = action != JOIN_HUMAN_GAME

        self.ai_group.setVisible(show_ai)
        self.lobby.setVisible(show_lobby)
        self.user_side.setVisible(show_side)
        self.board_type.setVisible(show_board_setup)

    def _on_board_type_changed(self, idx):
        action = self.board_type.itemData(idx)
        show_fen = action == LOAD_FEN
        self.fen_path.setVisible(show_fen)

    def get_settings(self):
        game = GameSettings()
        game.user_name = self.user_name.text()
        game.rules = self.rules.currentData()
        action = self.game_type.currentData()
        game.run_now = action != START_HUMAN_GAME
        side = self.user_side.currentData()
        game.user_turn_first = side == FIRST
        game.action = action
        game.board_setup = self.board_type.currentData() == MANUAL_BOARD
        if self.board_type.currentData() == LOAD_FEN:
            game.fen_path = self.fen_path.path()

        game.ai = ai = AI()
        ai.depth = self.ai_depth.currentData()
        #ai.start_depth = ai.depth - 2
        ai.load = self.ai_load.checkState() == Qt.Checked
        ai.store = self.ai_save.checkState() == Qt.Checked

        return game


