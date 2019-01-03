
import sys
from os.path import join, exists, dirname
import os
import logging

from PyQt5.QtGui import QPainter, QPixmap, QIcon
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QApplication, QWidget, QToolBar, QMainWindow, QDialog, QVBoxLayout, QAction, QActionGroup, QLabel, QFileDialog, QFrame, QDockWidget, QMessageBox, QListWidget, QListWidgetItem

from field import Field
from game import Game, AI, RequestError
from board import Board
from theme import Theme
from history import HistoryWidget
from newgamedlg import *
from settingsdlg import SettingsDialog

class UiLogHandler(logging.Handler):
    def __init__(self, list_widget):
        logging.Handler.__init__(self)
        self.list_widget = list_widget

    def get_icon(self, record):
        if record.levelno == logging.INFO:
            return QIcon.fromTheme("dialog-information")
        elif record.levelno == logging.ERROR:
            return QIcon.fromTheme("dialog-error")
        elif record.levelno == logging.WARNING:
            return QIcon.fromTheme("dialog-warning")
        return None

    def emit(self, record):
        try:
            msg = self.format(record)
            item = QListWidgetItem(self.list_widget)
            item.setText(msg)
            icon = self.get_icon(record)
            if icon is not None:
                item.setIcon(icon)
            self.list_widget.update()
            self.list_widget.scrollToBottom()
            self.flush()
        except Exception:
            self.handleError(record)

class Checkers(QMainWindow):
    def __init__(self, share_dir):
        QMainWindow.__init__(self)
        self.share_dir = share_dir
        self.setWindowTitle(_("HCheckers client"))
        self.settings = QSettings("hcheckers", "hcheckers")
        self._board_setup_mode = False
        self._game_active = False
        self._prepare()
        self._gui_setup()
        self._setup_actions()
        self._default_new_game()

    def get_board_setup_mode(self):
        return self._board_setup_mode

    def set_board_setup_mode(self,mode):
        self._board_setup_mode = mode
        self.run_action.setEnabled(mode)
        self.put_first_action.setEnabled(mode)
        self.put_second_action.setEnabled(mode)
        self.erase_action.setEnabled(mode)

    board_setup_mode = property(get_board_setup_mode, set_board_setup_mode)

    def get_my_turn(self):
        return self.board.my_turn

    def set_my_turn(self, value):
        self.board.my_turn = value
        if value:
            self.statusBar().showMessage(_("Your turn."))
        else:
            self.statusBar().showMessage(_("Awaiting a turn from another side."))

    my_turn = property(get_my_turn, set_my_turn)

    def get_game_active(self):
        return self._game_active

    def set_game_active(self, value):
        self._game_active = value
        self.board.locked = not value

    game_active = property(get_game_active, set_game_active)

    def _prepare(self):
        if self.share_dir is None:
            raise Exception("Cant locate share directory")
        theme_name = self.settings.value("theme", "default")
        self.theme = Theme(join(self.share_dir, "themes", theme_name), None)
        self.server_url = self.settings.value("server_url", DEFAULT_SERVER_URL)
        self.game = Game(self.server_url)
        self.poll_timer = self.startTimer(500)
        self.do_poll = False

    @handling_error
    def _gui_setup(self):
        widget = QWidget(self)
        layout = QVBoxLayout()
        self.board = Board(self.theme, self.settings, self.game, self)
        self.board.message.connect(self._on_board_message)
        self.board.field_clicked.connect(self._on_field_clicked)
        #self.board.show()
        self.toolbar = QToolBar(self)
        self.message = QLabel(self)
        layout.addWidget(self.toolbar)
        layout.addWidget(self.message)
        layout.addWidget(self.board, stretch=1)
        widget.setLayout(layout)
        self.setCentralWidget(widget)

        self.status_info = QLabel(self)
        self.statusBar().addPermanentWidget(self.status_info)
        self.rules_info = QLabel(self)
        self.rules_info.setFrameStyle(QFrame.Sunken | QFrame.Panel)
        #self.rules_info.setLineWidth(3)
        self.statusBar().addPermanentWidget(self.rules_info)
        self.opponent_info = QLabel(self)
        self.opponent_info.setFrameStyle(QFrame.Sunken | QFrame.Panel)
        #self.opponent_info.setLineWidth(3)
        self.statusBar().addPermanentWidget(self.opponent_info)

        self.history = HistoryWidget(self.game, self.board, self)
        self.history_dock = QDockWidget(_("History"), self)
        self.history_dock.setAllowedAreas(Qt.AllDockWidgetAreas)
        self.history_dock.setWidget(self.history)
        self.history_dock.setObjectName("history")
        self.addDockWidget(Qt.RightDockWidgetArea, self.history_dock)

        self.log = QListWidget(self)
        self.log_dock = QDockWidget(_("Log"), self)
        self.log_dock.setAllowedAreas(Qt.AllDockWidgetAreas)
        self.log_dock.setWidget(self.log)
        self.log_dock.setObjectName("log")
        self.addDockWidget(Qt.BottomDockWidgetArea, self.log_dock)

        console_handler = logging.getLogger().handlers[0]
        logging.getLogger().removeHandler(console_handler)
        log_handler = UiLogHandler(self.log)
        logging.getLogger().setLevel(logging.INFO)
        logging.getLogger("urllib3.connectionpool").setLevel(logging.WARNING)
        logging.getLogger().addHandler(log_handler)

        self.board.server_log.connect(self._on_server_log)

        geometry = self.settings.value("geometry")
        if geometry is not None:
            self.restoreGeometry(geometry)
        state = self.settings.value("windowState")
        if state is not None:
            self.restoreState(state)
    
    def _create_action(self, icon, title, menu, handler=None, group=None, toggle=False, toolbar=True, key=None):
        if group is None:
            parent = self
        else:
            parent = group
        action = QAction(title, parent)
        if icon is not None:
            action.setIcon(icon)
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
        menu = self.menuBar().addMenu(_("&Game"))
        self._create_action(QIcon.fromTheme("document-new"), _("&New Game"), menu, self._on_new_game, key="Ctrl+N")
        self._create_action(QIcon.fromTheme("document-save"), _("Save Position"), menu, self._on_save_game, key="Ctrl+S")
        self._create_action(QIcon.fromTheme("edit-undo"), _("&Undo"), menu, self._on_undo, key="Ctrl+Z")
        self._create_action(None, _("Capitulate"), menu, self._on_capitulate, toolbar=False)

        menu.addSeparator()
        self.toolbar.addSeparator()

        self.run_action = self._create_action(QIcon.fromTheme("media-playback-start"), _("Start &Game"), menu, self._on_run_game, key="Ctrl+R")
        menu.addSeparator()
        self._create_action(QIcon.fromTheme("preferences-system"), _("Se&ttings"), menu, self._on_settings, toolbar=False)

        menu = self.menuBar().addMenu(_("&Position"))
        setup = QActionGroup(self)
        setup.setExclusive(True)
        self.put_first_action = self._create_action(None, _("Put &white piece"), menu, group=setup, toggle=True)
        self.put_second_action = self._create_action(None, _("Put &black piece"), menu, group=setup, toggle=True)
        self.erase_action = self._create_action(QIcon.fromTheme("list-remove"), _("&Remove piece"), menu, group=setup, toggle=True)
        self.board_setup_mode = False
        menu.addSeparator()
        self.toolbar.addSeparator()

        menu = self.menuBar().addMenu(_("&View"))
        self.flip_action = self._create_action(QIcon.fromTheme("object-flip-vertical"), _("&Flip board"), menu, self._on_flip_board, toggle=True, key="Ctrl+T")
        flip = self.settings.value("flip_board", False, type=bool)
        self.flip_action.setChecked(flip)
        self._set_flip_board(flip)
        menu.addAction(self.history_dock.toggleViewAction())
        menu.addAction(self.log_dock.toggleViewAction())

    @handling_error
    def _on_run_game(self, checked=None):
        self.board_setup_mode = False
        board = self.board.json()
        self.game.start_new_game(self.game_settings.user_name, rules=self.game_settings.rules, user_turn_first=self.game_settings.user_turn_first, ai=self.game_settings.ai, board=board)
        self.board.fields_setup()

    @handling_error
    def _on_field_clicked(self, row, col):
        if not self.board_setup_mode:
            return

        first = self.put_first_action.isChecked()
        second = self.put_second_action.isChecked()
        erase = self.erase_action.isChecked()

        if not first and not second and not erase:
            return
        if first:
            side = FIRST
        elif second:
            side = SECOND

        piece = self.board.fields[(row,col)].piece
        if not erase:
            if piece and piece.side == side:
                if piece.kind == MAN:
                    piece.kind = KING
                else:
                    piece.kind = MAN
            else:
                piece = Piece(MAN, side)
        else:
            piece = None
        self.board.fields[(row,col)].piece = piece

    @handling_error
    def _default_new_game(self):
        self._on_new_game()

    @handling_error
    def _on_new_game(self, checked=None):
        dialog = NewGameDialog(self.settings, self.game, self)
        result = dialog.exec_()
        if result == QDialog.Accepted:
            if self.game.is_active():
                self.game.capitulate()
            self.game_active = True
            self.game.game_id = None
            self.game_settings = game = dialog.get_settings()
            if game.action == START_AI_GAME:
                if game.board_setup:
                    self.board.empty()
                    self.board_setup_mode = True
                    self.game.rules = game.rules
                else:
                    self.game.start_new_game(game.user_name, rules=game.rules, user_turn_first=game.user_turn_first, ai=game.ai, fen_path=game.fen_path, pdn_path=game.pdn_path, previous_board_game=game.previous_board_game)
                    state = self.game.get_state()
                    my_side = 'First' if self.game.user_side == FIRST else 'Second'
                    self.my_turn = state["side"] == my_side
                    self.rules_info.setText(_("Rules: {}").format(rules_dict[game.rules]))
                    self.opponent_info.setText(_("AI: {}").format(game.ai.title))
                    self.status_info.setText("")
            elif game.action == START_HUMAN_GAME:
                game_id = self.game.new_game(game.rules)
                logging.info(_("New game ID: {}").format(game_id))
                if game.user_turn_first:
                    self.game.register_user(game.user_name, FIRST)
                else:
                    self.game.register_user(game.user_name, SECOND)
                self.do_poll = True
                self.rules_info.setText(_("Rules: {}").format(game.rules))
                self.opponent_info.setText("")
                self.status_info.setText("")
                self.game_active = False
                self.statusBar().showMessage(_("Waiting for another side to join the game."))
            elif game.action == JOIN_HUMAN_GAME:
                self.game.game_id = dialog.lobby.get_game_id()
                self.game.user_side = side = dialog.lobby.get_free_side()
                self.game.rules = dialog.lobby.get_rules()
                #used_name = dialog.lobby.get_used_name()
                self.game.register_user(game.user_name, side)
                self.game.run_game()
                self.do_poll = True
                self.my_turn = side == FIRST
                self.rules_info.setText(_("Rules: {}").format(game.rules))
                self.opponent_info.setText("")
                self.status_info.setText("")

            size, invert, notation = self.game.get_notation(game.rules)
            self.board.invert_colors = invert
            self.board.set_notation(size, notation)

            self.board.repaint()
            self.history.fill()

    @handling_error
    def _on_save_game(self, checked=None):
        (path,mask) = QFileDialog.getSaveFileName(self.board, _("Save file"), ".", FEN_MASK + ";;" + PDN_MASK)
        if path:
            if mask == FEN_MASK:
                fen = self.game.get_fen()
                with open(path, 'w') as f:
                    f.write(fen)
            elif mask == PDN_MASK:
                pdn = self.game.get_pdn()
                with open(path, 'w') as f:
                    f.write(pdn)

    @handling_error
    def _on_undo(self, checked=None):
        prev_board = self.game.undo()
        self.board.fields_setup(prev_board)
        self.board.repaint()
        self.history.fill()

    @handling_error
    def _on_capitulate(self, checked=None):
        messages = self.game.capitulate()
        for message in messages:
            self.board.process_message(message)
        #self.my_turn = False

    @handling_error
    def get_result_str(self, result):
        first, second = self.game.get_colors(self.game.rules)
        if result == 'FirstWin':
            return _("{} win").format(first)
        elif result == 'SecondWin':
            return _("{} win").format(second)
        else:
            return _("Draw")

    def _on_board_message(self, message):
        if isinstance(message, GameResultMessage):
            self.game_active = False
            result = self.get_result_str(message.result)
            self.status_info.setText(_("Game result: {}").format(result))
            self.board.setCursor(Qt.ArrowCursor)
            self.statusBar().showMessage(_("Game over."))
            self.history.fill()
        elif isinstance(message, OtherSideMove):
            self.message.setText(unicode(message))
            self.history.fill()
            self.my_turn = True
        elif isinstance(message, WaitingMove):
            self.statusBar().showMessage(unicode(message))

    def _on_server_log(self, level, message):
        item = QListWidgetItem(self.log)
        item.setText(message)
        icon = None
        if level == "INFO":
            icon = QIcon.fromTheme("dialog-information")
        elif level == "ERROR":
            icon = QIcon.fromTheme("dialog-error")
        elif level == "WARNING":
            icon = QIcon.fromTheme("dialog-warning")
        if icon is not None:
            item.setIcon(icon)
        self.log.update()
        self.log.scrollToBottom()

    def _set_flip_board(self, value):
        self.board.flip = value
        self.settings.setValue("flip_board", self.board.flip)

    def _on_flip_board(self):
        self._set_flip_board(self.flip_action.isChecked())

    def _on_settings(self):
        dialog = SettingsDialog(self.settings, self.share_dir, self)
        result = dialog.exec_()
        if result == QDialog.Accepted:
            self.board.show_possible_moves = dialog.get_show_possible_moves()
            self.board.show_notation = dialog.get_show_notation()
            self.board.theme = dialog.get_theme()
            logging.info("ok")

    def _handle_game_error(self, rs):
        message = _("Unexpected response received from the server.\nRequest URL: {}\nResponse code: {}\nResponse message: {}").format(rs.url, rs.status_code, rs.text)
        QMessageBox.critical(self, _("Exception"), message)
        logging.exception(message)

    def _handle_connection_error(self, url, e):
        message = _("An exception occured while connecting to the server.\nRequest URL: {}\nException text: {}").format(url, e)
        QMessageBox.critical(self, _("Exception"), message)
        logging.exception(message)

    def closeEvent(self, ev):
        
        if self.game.is_active():
            try:
                self.game.capitulate()
            except Exception as e:
                logging.exception(e)
                print(e)

        self.settings.setValue("geometry", self.saveGeometry())
        self.settings.setValue("windowState", self.saveState())
        QMainWindow.closeEvent(self, ev)

    @handling_error
    def timerEvent(self, e):
        if e.timerId() != self.poll_timer:
            return
        #if self.my_turn:
        #    return

        if self.do_poll and not self.game_active:
            state = self.game.get_state()
            if state["status"] == 'Running':
                self.game_active = True
                my_side = 'First' if self.game.user_side == FIRST else 'Second'
                self.my_turn = state['side'] == my_side
                #self.statusBar().clearMessage()

        if not self.game.is_active():
            return

        if self.do_poll:
            board, messages = self.game.poll()
            for message in messages:
                self.board.process_message(message)
                self.my_turn = True
            self.board.fields_setup(board)
        else:
            board, messages = self.game.poll()
            for message in messages:
                if "message" in message:
                    text = message["message"]
                    level = message["level"]
                    self._on_server_log(level, text)



