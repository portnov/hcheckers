
import getpass
import logging

from PyQt5.QtGui import QPainter, QPixmap, QValidator, QIcon
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QWidget, QDialog, QPushButton, QVBoxLayout, \
        QHBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, \
        QDialogButtonBox, QFileDialog, QLabel, QApplication

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
RANDOM_PRESET = 6

NO_TIMING = "NO_TIMING"

FEN_MASK = "FEN notation (*.fen)"
PDN_MASK = "Portable Draughts Notation (*.pdn)"

EXIT = 2

def select_game_file(widget):
    any_mask = FEN_MASK + ";;" + PDN_MASK
    (path,mask) = QFileDialog.getOpenFileName(widget, _("Open file"), ".", any_mask)
    return path, mask

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

    selected = pyqtSignal(str, str)

    def _on_browse(self):
        (path,mask) = QFileDialog.getOpenFileName(self, _("Open file"), ".", self.mask)
        self.textbox.setText(path)
        self.selected.emit(path, mask)

    def path(self):
        return self.textbox.text()

    def setPath(self, path):
        self.textbox.setText(path)

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
    def __init__(self, settings, client, share_dir, show_exit=False, open_file=(None,None), parent=None):
        DialogBase.__init__(self, parent)
        self.settings = settings
        self.client = client
        self.share_dir = share_dir
        self.show_exit = show_exit
        self.preset_file, self.preset_mask = open_file

        self.setWindowTitle(_("Start new game"))

        widget = QWidget()
        self.form_layout = layout = QFormLayout()

        self.game_type = QComboBox(self)
        self.game_type.addItem(_("Start a game against computer"), START_AI_GAME)
        self.game_type.addItem(_("Start a game against human"), START_HUMAN_GAME)
        self.game_type.addItem(_("Join a game against human"), JOIN_HUMAN_GAME)
        layout.addRow(_("Action"), self.game_type)

        self.rules = QComboBox()
        for name, title, link in supported_rules:
            self.rules.addItem(title, name)
        rules = settings.value("rules")
        if rules is not None:
            idx = self.rules.findData(rules)
            self.rules.setCurrentIndex(idx)
        self.rules.currentIndexChanged.connect(self._on_select_rules)
        rules_box = QHBoxLayout()
        rules_box.addWidget(self.rules, 1)
        rules_help = QPushButton(self)
        rules_help.setToolTip(_("Click to open the description of selected rules"))
        rules_help.setIcon(QIcon.fromTheme("help-contents"))
        rules_help.clicked.connect(self._on_rules_help)
        rules_box.addWidget(rules_help)
        layout.addRow(_("Rules"), rules_box)

        self.timing = QComboBox()
        have_timing_options = self._fill_timing_options()
        timing = settings.value("timing")
        if timing is not None:
            idx = self.timing.findData(timing)
            self.timing.setCurrentIndex(idx)
        if have_timing_options:
            layout.addRow(_("Time control"), self.timing)

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
        self.board_type.addItem(_("Use default initial position"), DEFAULT_BOARD)         # 0
        self.board_type.addItem(_("Manual initial position setup"), MANUAL_BOARD)         # 1
        self.board_type.addItem(_("Use initial board of previous game"), PREVIOUS_BOARD)  # 2
        self.board_type.addItem(_("Load initial board from FEN file"), LOAD_FEN)          # 3
        self.board_type.addItem(_("Load initial board from PDN file"), LOAD_PDN)          # 4
        self.board_type.addItem(_("Use random initial board preset"), RANDOM_PRESET)      # 5
        layout.addRow(_("Initial board type"), self.board_type)

        self.file_path = FileSelectWidget(self)
        self.file_path.setVisible(False)
        #self.file_path.selected.connect(self._on_file_selected)
        layout.addRow(_("Select file"), self.file_path)
        layout.labelForField(self.file_path).setVisible(False)

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
        self.ok_button.setToolTip(_("Start a new game"))
        cancel_button = buttons.button(QDialogButtonBox.Cancel)
        cancel_button.setToolTip(_("Close this dialog without starting a new game"))

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
        refresh.clicked.connect(self._on_refresh)
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

        if self.show_exit:
            exit = QPushButton(_("E&xit"), self)
            exit.setIcon(QIcon.fromTheme("application-exit"))
            exit.setToolTip(_("Close this dialog and exit the program"))
            exit.clicked.connect(self._on_exit)
            buttons_box.addWidget(exit)

        vbox.addLayout(buttons_box)

        if self.preset_file:
            if self.preset_mask == PDN_MASK:
                self.board_type.setCurrentIndex(4)
            elif self.preset_mask == FEN_MASK:
                self.board_type.setCurrentIndex(3)
            self.file_path.mask = self.preset_mask
            self.file_path.setPath(self.preset_file)

    def _fill_ais(self, settings=None):
        if settings is None:
            settings = self.settings
        self.ai.clear()
        ai_list = []
        if self.client is not None:
            ai_list.extend(self.client.get_ai_settings())
        enable_custom = True
        if self.client is not None:
            enable_custom = self.client.is_custom_ai_settings_enabled()
        if enable_custom:
            ai_list.extend(AI.list_from_settings(self.share_dir, settings))
        self.ais = ai_list
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

    def _on_refresh(self):
        self.lobby.fill()
        self._fill_ais()

    def _on_exit(self):
        self.done(EXIT)

    def _on_settings(self):
        dialog = SettingsDialog(self.settings, self.share_dir, parent=self, client=self.client)
        result = dialog.exec_()
        if result == QDialog.Accepted:
            self.settings.sync()
            self._fill_ais(self.settings)
            self.client.base_url = self.settings.value("server_url", DEFAULT_SERVER_URL)
            level = self.settings.value("log_level", logging.INFO, type=int)
            logging.getLogger().setLevel(level)
            logging.info(_("Settings have been updated."))

    def _on_action_changed(self, idx):
        action = self.game_type.itemData(idx)
        board_type = self.board_type.itemData(idx)

        show_ai = action == START_AI_GAME
        show_lobby = action == JOIN_HUMAN_GAME or board_type == PREVIOUS_BOARD
        show_side = action != JOIN_HUMAN_GAME
        show_board_setup = action != JOIN_HUMAN_GAME

        self.ai.setVisible(show_ai)
        self.form_layout.labelForField(self.ai).setVisible(show_ai)
        self.lobby.setVisible(show_lobby)
        self.refresh_button.setVisible(show_lobby)

        self.user_side.setVisible(show_side)
        self.form_layout.labelForField(self.user_side).setVisible(show_side)
        self.board_type.setVisible(show_board_setup)
        self.form_layout.labelForField(self.board_type).setVisible(show_board_setup)

    def _on_board_type_changed(self, idx):
        action = self.game_type.currentData()
        board_type = self.board_type.itemData(idx)
        show_file = board_type == LOAD_FEN or board_type == LOAD_PDN
        self.file_path.setVisible(show_file)
        self.form_layout.labelForField(self.file_path).setVisible(show_file)

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

    def _fill_timing_options(self):
        result = False
        rules = self.rules.currentData()
        if self.client is not None:
            self.timing.clear()
            self.timing.addItem(_("Do not use"), NO_TIMING)
            for slug, title in self.client.get_timing_options(rules):
                self.timing.addItem(title, slug)
                result = True
        return result

    def _on_select_rules(self, event=None):
        self._fill_timing_options()

    def _on_rules_help(self):
        rules = self.rules.currentData()
        open_rules_help(rules)

    def _on_accept(self):
        self.settings.setValue("ai", self.ai.currentText())
        self.settings.setValue("rules", self.rules.currentData())
        self.settings.setValue("timing", self.timing.currentData())
        self.settings.setValue("user_side", self.user_side.currentIndex())
        self.accept()

    def get_settings(self):
        game = GameSettings()
        game.user_name = self.user_name.widget.text()
        game.rules = self.rules.currentData()
        timing = self.timing.currentData()
        if timing == NO_TIMING:
            timing = None
        game.timing = timing
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
        elif board_type == RANDOM_PRESET:
            game.use_random_board_preset = True

        game.ai = self.ais[self.ai.currentIndex()]

        return game

