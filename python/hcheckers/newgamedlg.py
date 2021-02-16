
import getpass
import logging

from PyQt5.QtGui import QPainter, QPixmap, QValidator, QIcon
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QWidget, QDialog, QPushButton, QVBoxLayout, \
        QHBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, \
        QDialogButtonBox, QFileDialog, QLabel

from hcheckers.common import *
from hcheckers.game import AI, GameSettings
from hcheckers.lobby import LobbyWidget
from hcheckers.settingsdlg import SettingsDialog

START_AI_GAME = 1
START_HUMAN_GAME = 2
JOIN_HUMAN_GAME = 3

DEFAULT_BOARD = 1
MANUAL_BOARD = 2
LOAD_FEN = 3
LOAD_PDN = 4
PREVIOUS_BOARD = 5

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

class NewGameDialog(DialogBase):
    def __init__(self, settings, client, share_dir, show_exit=False, parent=None):
        DialogBase.__init__(self, parent)
        self.settings = settings
        self.client = client
        self.share_dir = share_dir
        self.show_exit = show_exit

        widget = QWidget()
        self.form_layout = layout = QFormLayout()

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

        self.user_name = MandatoryField(_("User name"), QLineEdit(self))
        self.user_name.widget.setText(getpass.getuser())
        self.user_name_validator = NameValidator(self)
        self.user_name.widget.setValidator(self.user_name_validator)
        self.user_name.add_to_form(layout)

        self.user_side = QComboBox(self)
        self.user_side.addItem(_("White"), FIRST)
        self.user_side.addItem(_("Black"), SECOND)
        layout.addRow(_("User plays"), self.user_side)
        user_side = settings.value("user_side", type=int)
        if user_side is not None:
            self.user_side.setCurrentIndex(user_side)

        self.board_type = QComboBox(self)
        self.board_type.addItem(_("Use default initial position"), DEFAULT_BOARD)
        self.board_type.addItem(_("Manual initial position setup"), MANUAL_BOARD)
        self.board_type.addItem(_("Use initial board of previous game"), PREVIOUS_BOARD)
        self.board_type.addItem(_("Load initial board from FEN file"), LOAD_FEN)
        self.board_type.addItem(_("Load initial board from PDN file"), LOAD_PDN)
        layout.addRow(_("Initial board type"), self.board_type)

        self.file_path = FileSelectWidget(self)
        self.file_path.setVisible(False)
        layout.addRow(_("Select file"), self.file_path)

        widget.setLayout(layout)

        vbox = QVBoxLayout()
        vbox.addWidget(widget)

        self.message_label = QLabel(self)
        self.message_label.hide()

        buttons = QDialogButtonBox(
                    QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
                    Qt.Horizontal, self)
        buttons.accepted.connect(self._on_accept)
        buttons.rejected.connect(self.reject)
        self.ok_button = buttons.button(QDialogButtonBox.Ok)

        self.ai = QComboBox(self)
        self._fill_ais(settings)
        layout.addRow(_("AI"), self.ai)

        lobby_hbox = QHBoxLayout()

        self.lobby = LobbyWidget(client=client, parent=self)
        self.lobby.selected.connect(self._on_game_selected)
        self.lobby.hide()
        lobby_hbox.addWidget(self.lobby)

        self.refresh_button = refresh = QPushButton(_("Refresh"), self)
        refresh.setIcon(QIcon.fromTheme("view-refresh"))
        refresh.clicked.connect(self.lobby.fill)
        refresh.hide()
        lobby_hbox.addWidget(refresh)

        self.lobby.fill()

        vbox.addLayout(lobby_hbox)
        self.setLayout(vbox)

        self.game_type.currentIndexChanged.connect(self._on_action_changed)
        self.board_type.currentIndexChanged.connect(self._on_board_type_changed)

        vbox.addWidget(self.message_label)

        settings_btn = QPushButton(self)
        settings_btn.setIcon(QIcon.fromTheme("preferences-system"))
        settings_btn.setToolTip(_("Open settings dialog"))
        settings_btn.clicked.connect(self._on_settings)

        buttons_box = QHBoxLayout()
        buttons_box.addWidget(settings_btn)
        buttons_box.addStretch()
        buttons_box.addWidget(buttons)
        vbox.addLayout(buttons_box)

    def _fill_ais(self, settings):
        self.ai.clear()
        self.ais = AI.list_from_settings(settings)
        for idx, ai in enumerate(self.ais):
            self.ai.addItem(ai.title, idx)
        ai = settings.value("ai")
        if ai is not None:
            idx = self.ai.findText(ai)
            self.ai.setCurrentIndex(idx)

    def message(self, text):
        self.message_label.setVisible(bool(text))
        if text:
            self.message_label.setText(text)

    def get_ok_button(self):
        return self.ok_button

    def get_form_layout(self):
        return self.form_layout

    def _on_settings(self):
        dialog = SettingsDialog(self.settings, self.share_dir, self)
        result = dialog.exec_()
        if result == QDialog.Accepted:
            self.settings.sync()
            self._fill_ais(self.settings)
            logging.info(_("Settings have been updated."))

    def _on_action_changed(self, idx):
        action = self.game_type.itemData(idx)
        board_type = self.board_type.itemData(idx)

        show_ai = action == START_AI_GAME
        show_lobby = action == JOIN_HUMAN_GAME or board_type == PREVIOUS_BOARD
        show_side = action != JOIN_HUMAN_GAME
        show_board_setup = action != JOIN_HUMAN_GAME

        self.ai.setVisible(show_ai)
        self.lobby.setVisible(show_lobby)
        self.refresh_button.setVisible(show_lobby)
        self.user_side.setVisible(show_side)
        self.board_type.setVisible(show_board_setup)

    def _on_board_type_changed(self, idx):
        action = self.game_type.currentData()
        board_type = self.board_type.itemData(idx)
        show_file = board_type == LOAD_FEN or board_type == LOAD_PDN
        self.file_path.setVisible(show_file)

        show_rules = board_type != LOAD_PDN
        self.rules.setVisible(show_rules)

        show_lobby = action == JOIN_HUMAN_GAME or board_type == PREVIOUS_BOARD
        self.lobby.setVisible(show_lobby)
        self.lobby.set_selectable(board_type != PREVIOUS_BOARD)

        if board_type == LOAD_FEN:
            self.file_path.mask = FEN_MASK
        elif board_type == LOAD_PDN:
            self.file_path.mask = PDN_MASK

    def _on_game_selected(self, game):
        action = self.game_type.currentData()
        if action == JOIN_HUMAN_GAME:
            used_name = game.get_used_name()
            if used_name is None:
                return
            self.user_name_validator.used_name = used_name
            name = self.user_name.widget.text()
            self.user_name.widget.setText(self.user_name_validator.fixup(name))

    def _on_accept(self):
        self.settings.setValue("ai", self.ai.currentText())
        self.settings.setValue("rules", self.rules.currentData())
        self.settings.setValue("user_side", self.user_side.currentIndex())
        self.accept()

    def get_settings(self):
        game = GameSettings()
        game.user_name = self.user_name.widget.text()
        game.rules = self.rules.currentData()
        action = self.game_type.currentData()
        game.run_now = action != START_HUMAN_GAME
        side = self.user_side.currentData()
        board_type = self.board_type.currentData()
        game.user_turn_first = side == FIRST
        if self.client.get_invert_colors(game.rules):
            game.user_turn_first = not game.user_turn_first
        game.action = action
        game.board_type = board_type

        game.board_setup = board_type == MANUAL_BOARD
        if board_type == LOAD_FEN:
            game.fen_path = self.file_path.path()
        elif board_type == LOAD_PDN:
            game.pdn_path = self.file_path.path()
        elif board_type == PREVIOUS_BOARD:
            game.previous_board_game = self.lobby.get_game_id()

        game.ai = self.ais[self.ai.currentIndex()]

        return game

