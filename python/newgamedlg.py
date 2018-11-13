
import getpass

from PyQt5.QtGui import QPainter, QPixmap, QValidator
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
LOAD_PDN = 4

FEN_MASK = "FEN notation (*.fen)"
PDN_MASK = "Portable Draughts Notation (*.pdn)"

class FileSelectWidget(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        layout = QHBoxLayout()
        self.textbox = QLineEdit(self)
        self.button = QPushButton(_("Browse..."))
        self.button.clicked.connect(self._on_browse)
        layout.addWidget(self.textbox)
        layout.addWidget(self.button)
        self.setLayout(layout)
        self.mask = FEN_MASK

    def _on_browse(self):
        (path,mask) = QFileDialog.getOpenFileName(self, _("Open file"), ".", self.mask)
        self.textbox.setText(path)

    def path(self):
        return self.textbox.text()

class NameValidator(QValidator):
    def __init__(self, used_name, parent=None):
        QValidator.__init__(self, parent)
        self.used_name = used_name

    def validate(self, name, pos):
        if name == self.used_name:
            return (QValidator.Invalid, name, pos)
        else:
            return (QValidator.Acceptable, name, pos)

    def fixup(self, name):
        if name == self.used_name:
            return name + "_2"
        else:
            return name

class NewGameDialog(QDialog):
    def __init__(self, settings, client, parent=None):
        QDialog.__init__(self, parent)
        self.settings = settings
        self.client = client

        widget = QWidget()
        layout = QFormLayout()

        self.game_type = QComboBox(self)
        self.game_type.addItem(_("Start a game against computer"), START_AI_GAME)
        self.game_type.addItem(_("Start a game against human"), START_HUMAN_GAME)
        self.game_type.addItem(_("Join a game against human"), JOIN_HUMAN_GAME)
        layout.addRow(_("Action"), self.game_type)

        self.rules = QComboBox()
        for name, title in supported_rules:
            self.rules.addItem(title, name)
        rules = settings.value("rules")
        if rules is not None:
            idx = self.rules.findData(rules)
            self.rules.setCurrentIndex(idx)
        layout.addRow(_("Rules"), self.rules)

        self.user_name = QLineEdit(self)
        self.user_name.setText(getpass.getuser())
        self.user_name_validator = NameValidator(self)
        self.user_name.setValidator(self.user_name_validator)
        layout.addRow(_("User name"), self.user_name)

        self.user_side = QComboBox(self)
        self.user_side.addItem(_("White"), FIRST)
        self.user_side.addItem(_("Black"), SECOND)
        layout.addRow(_("User plays"), self.user_side)

        self.board_type = QComboBox(self)
        self.board_type.addItem(_("Use default initial position"), DEFAULT_BOARD)
        self.board_type.addItem(_("Manual initial position setup"), MANUAL_BOARD)
        self.board_type.addItem(_("Load initial board from FEN file"), LOAD_FEN)
        self.board_type.addItem(_("Load initial board from PDN file"), LOAD_PDN)
        layout.addRow(_("Initial board type"), self.board_type)

        self.file_path = FileSelectWidget(self)
        self.file_path.setVisible(False)
        layout.addRow(_("Select file"), self.file_path)

        widget.setLayout(layout)

        vbox = QVBoxLayout()
        vbox.addWidget(widget)

        self.ai = QComboBox(self)
        self.ais = AI.list_from_settings(settings)
        for idx, ai in enumerate(self.ais):
            self.ai.addItem(ai.title, idx)
        ai = settings.value("ai")
        if ai is not None:
            idx = self.ai.findText(ai)
            self.ai.setCurrentIndex(idx)
        layout.addRow(_("AI"), self.ai)

        self.lobby = LobbyWidget(client=client, parent=self)
        self.lobby.selected.connect(self._on_game_selected)
        self.lobby.hide()
        vbox.addWidget(self.lobby)

        self.setLayout(vbox)

        self.game_type.currentIndexChanged.connect(self._on_action_changed)
        self.board_type.currentIndexChanged.connect(self._on_board_type_changed)

        buttons = QDialogButtonBox(
                    QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
                    Qt.Horizontal, self)
        buttons.accepted.connect(self._on_accept)
        buttons.rejected.connect(self.reject)
        vbox.addWidget(buttons)

    def _on_action_changed(self, idx):
        action = self.game_type.itemData(idx)

        show_ai = action == START_AI_GAME
        show_lobby = action == JOIN_HUMAN_GAME
        show_side = action != JOIN_HUMAN_GAME
        show_board_setup = action != JOIN_HUMAN_GAME

        self.ai.setVisible(show_ai)
        self.lobby.setVisible(show_lobby)
        self.user_side.setVisible(show_side)
        self.board_type.setVisible(show_board_setup)

    def _on_board_type_changed(self, idx):
        action = self.board_type.itemData(idx)
        show_file = action == LOAD_FEN or action == LOAD_PDN
        self.file_path.setVisible(show_file)

        show_rules = action != LOAD_PDN
        self.rules.setVisible(show_rules)

        if action == LOAD_FEN:
            self.file_path.mask = FEN_MASK
        elif action == LOAD_PDN:
            self.file_path.mask = PDN_MASK

    def _on_game_selected(self, game):
        used_name = game.get_used_name()
        if used_name is None:
            return
        self.user_name_validator.used_name = used_name
        name = self.user_name.text()
        self.user_name.setText(self.user_name_validator.fixup(name))

    def _on_accept(self):
        self.settings.setValue("ai", self.ai.currentText())
        self.settings.setValue("rules", self.rules.currentData())
        self.accept()

    def get_settings(self):
        game = GameSettings()
        game.user_name = self.user_name.text()
        game.rules = self.rules.currentData()
        action = self.game_type.currentData()
        game.run_now = action != START_HUMAN_GAME
        side = self.user_side.currentData()
        game.user_turn_first = side == FIRST
        if self.client.get_invert_colors(game.rules):
            game.user_turn_first = not game.user_turn_first
        game.action = action
        game.board_setup = self.board_type.currentData() == MANUAL_BOARD
        if self.board_type.currentData() == LOAD_FEN:
            game.fen_path = self.file_path.path()
        elif self.board_type.currentData() == LOAD_PDN:
            game.pdn_path = self.file_path.path()

        game.ai = self.ais[self.ai.currentIndex()]

        return game

