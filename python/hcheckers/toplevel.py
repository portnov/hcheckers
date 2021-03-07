
import sys
from os.path import join, exists, dirname
import os
import re
import logging
import subprocess
import shlex
import time

from PyQt5.QtGui import QPainter, QPixmap, QIcon
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QApplication, QWidget, QToolBar, QMainWindow, QDialog, QVBoxLayout, QAction, QActionGroup, QLabel, QFileDialog, QFrame, QDockWidget, QMessageBox, QListWidget, QListWidgetItem, QMenu, QSplashScreen

from hcheckers.field import Field
from hcheckers.common import Move
from hcheckers.game import Game, AI, RequestError
from hcheckers.board import Board
from hcheckers.theme import Theme
from hcheckers.history import HistoryWidget
from hcheckers.newgamedlg import *
from hcheckers.settingsdlg import SettingsDialog
from hcheckers.logutils import *

class Checkers(QMainWindow):
    def __init__(self, share_dir):
        QMainWindow.__init__(self)
        self.share_dir = share_dir
        self.setWindowTitle(_("HCheckers client"))
        self.settings = QSettings("hcheckers", "hcheckers")
        self._board_setup_mode = False
        self._game_active = False
        self._connection_failed = False
        self._poll_try_number = 0
        self.splashscreen = None
        self._show_splashcreen(_("Starting HCheckers..."))
        self.server_url = self.settings.value("server_url", DEFAULT_SERVER_URL)
        self._start_server()
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

    def _start_server(self):
        server_running = Game.check_server(self.server_url)
        use_local_server = self.settings.value("use_local_server", type=bool)
        self.local_server_used = False
        if server_running:
            if use_local_server:
                logging.info(_("Server appears to be already running, do not start a local server"))
        else:
            if use_local_server:
                self.splashscreen.showMessage(_("Starting local server..."))
                QApplication.processEvents()
                server_path = self.settings.value("local_server_path")
                logging.info(_("Running local server: {}").format(server_path))
                server = subprocess.Popen(server_path, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                time.sleep(1)
                server.poll()
                if server.returncode is not None and server.returncode != 0:
                    output = server.stdout.read()
                    message = _("Could not start local server; exit code: {}; message:\n{}").format(server.returncode, output.decode('utf-8'))
                    logging.error(message)
                    QMessageBox.critical(self, _("Exception"), message)
                else:
                    self.local_server_used = True

    def _prepare(self):
        if self.share_dir is None:
            raise Exception("Cant locate share directory")
        theme_name = self.settings.value("theme", "default")
        self.theme = Theme(join(self.share_dir, "themes", theme_name), None)
        self.theme.enable_sound = self.settings.value("enable_sound", type=bool)
        self.game = Game(self.server_url)
        self.poll_timer = self.startTimer(500)
        self.setup_fields_on_poll = False

    def _icon(self, name):
        return QIcon(join(self.share_dir, "icons", name))

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
        self.log.setContextMenuPolicy(Qt.CustomContextMenu)
        self.log.customContextMenuRequested.connect(self._on_log_context_menu)
        self.log_dock = QDockWidget(_("Log"), self)
        self.log_dock.setAllowedAreas(Qt.AllDockWidgetAreas)
        self.log_dock.setWidget(self.log)
        self.log_dock.setObjectName("log")
        self.addDockWidget(Qt.BottomDockWidgetArea, self.log_dock)

        console_handler = logging.getLogger().handlers[0]
        logging.getLogger().removeHandler(console_handler)
        log_handler = UiLogHandler(self.log)
        logging.getLogger().setLevel(logging.INFO)
        logging.getLogger().addHandler(log_handler)
        for logger in lowered_loggers:
            logging.getLogger(logger).addFilter(LogFilter(lowered_regexps=lowered_regexps))

        self.board.server_log.connect(self._on_server_log)

        geometry = self.settings.value("UI/geometry")
        if geometry is not None:
            self.restoreGeometry(geometry)
        state = self.settings.value("UI/windowState")
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
        self._create_action(QIcon.fromTheme("document-open"), _("&Open Game..."), menu, self._on_open_game, key="Ctrl+O")
        self._create_action(QIcon.fromTheme("document-save"), _("&Save Position"), menu, self._on_save_game, key="Ctrl+S")
        self._create_action(QIcon.fromTheme("edit-undo"), _("&Undo"), menu, self._on_undo, key="Ctrl+Z")
        self.request_draw_action = self._create_action(self._icon("draw_offer.svg"), _("Offer a &draw"), menu, self._on_draw_rq)
        self.capitulate_action = self._create_action(self._icon("handsup.svg"), _("Capitulate"), menu, self._on_capitulate)

        menu.addSeparator()

        self.clear_log_action = self._create_action(QIcon.fromTheme("edit-clear"), _("&Clear log"), menu, self._on_clear_log, toolbar=False)
        self.copy_log_action = self._create_action(QIcon.fromTheme("edit-copy"), _("Copy selected log record"), menu, self._on_copy_log, toolbar=False)
        self.save_log_action = self._create_action(QIcon.fromTheme("document-save"), _("Save &log..."), menu, self._on_save_log, toolbar=False)

        menu.addSeparator()
        self.toolbar.addSeparator()

        self.run_action = self._create_action(QIcon.fromTheme("media-playback-start"), _("Start &Game"), menu, self._on_run_game, key="Ctrl+R")
        menu.addSeparator()
        self._create_action(QIcon.fromTheme("preferences-system"), _("Se&ttings"), menu, self._on_settings, toolbar=False)
        menu.addSeparator()
        self._create_action(QIcon.fromTheme("application-exit"), _("E&xit"), menu, self._on_exit, toolbar=False, key="Ctrl+Q")

        menu = self.menuBar().addMenu(_("&Position"))
        setup = QActionGroup(self)
        setup.setExclusive(True)
        self.put_first_action = self._create_action(self._icon("manwhite.svg"), _("Put &white piece"), menu, group=setup, toggle=True)
        self.put_second_action = self._create_action(self._icon("manblack.svg"), _("Put &black piece"), menu, group=setup, toggle=True)
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
        self.board.text_message = None
        self.board.fields_setup()

    @handling_error
    def _on_field_clicked(self, row, col):
        if not self.board_setup_mode:
            return

        field = self.board.fields[(row,col)]
        if not field.usable:
            logging.debug("You cant put piece at this field")
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

        piece = field.piece
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
        self.splashscreen.finish(self)
        if len(sys.argv) == 2:
            path = sys.argv[1]
            if path.endswith(".pdn"):
                mask = PDN_MASK
            elif path.endswith(".fen"):
                mask = FEN_MASK
            else:
                mask = None
        else:
            path, mask = None, None
        self._on_new_game(show_exit=True, open_file=(path,mask))

    @handling_error
    def _on_exit(self, *args):
        self.close()

    def _screen_size(self):
        rect = QApplication.desktop().availableGeometry(self)
        return min(rect.width(), rect.height())

    def _splashscreen_size(self):
        screen_size = self._screen_size()
        return screen_size / 2

    def _show_splashcreen(self, message=None):
        splash_size = self._splashscreen_size()
        splash_pix = self._icon("splashscreen.svg").pixmap(QSize(splash_size, splash_size))
        self.splashscreen = QSplashScreen(splash_pix, Qt.WindowStaysOnTopHint)
        self.splashscreen.show()
        QApplication.processEvents()
        if message is not None:
            self.splashscreen.showMessage(message)
            QApplication.processEvents()

    def _new_game(self, dialog):
        # Show splashcreen after user pressed Ok in the "new game" dialog
        self._show_splashcreen(_("Starting new game..."))

        if self.game.is_active():
            self.game.capitulate()
        self.board.text_message = None
        self.game_active = True
        self.game.game_id = None

        self.request_draw_action.setEnabled(True)
        self.capitulate_action.setEnabled(True)

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
            self.setup_fields_on_poll = True
            self.rules_info.setText(_("Rules: {}").format(game.rules))
            self.opponent_info.setText("")
            self.status_info.setText("")
            self.game_active = False
            message = _("Waiting for another side to join the game.")
            self.statusBar().showMessage(message)
            self.board.text_message = message
        elif game.action == JOIN_HUMAN_GAME:
            self.game.game_id = dialog.lobby.get_game_id()
            self.game.user_side = side = dialog.lobby.get_free_side()
            self.game.rules = dialog.lobby.get_rules()
            #used_name = dialog.lobby.get_used_name()
            self.game.register_user(game.user_name, side)
            self.game.run_game()
            self.setup_fields_on_poll = True
            self.my_turn = side == FIRST
            self.rules_info.setText(_("Rules: {}").format(game.rules))
            self.opponent_info.setText("")
            self.status_info.setText("")

        size, invert, notation = self.game.get_notation(game.rules)
        self.board.invert_colors = invert
        self.board.topology = self.game.get_topology(game.rules)
        self.board.set_notation(size, notation)

        self.board.theme = self.board.theme
        #self.board.repaint()
        self.history.fill()

    @handling_error
    def _on_new_game(self, checked=None, show_exit=False, open_file=(None,None)):
        dialog = NewGameDialog(self.settings, self.game, self.share_dir, show_exit, open_file=open_file, parent=self)
        result = dialog.exec_()

        if result == QDialog.Accepted:
            self._new_game(dialog)

        if self.splashscreen:
            self.splashscreen.finish(self)

        if result == EXIT:
            print("Exit!")
            #QApplication.quit()
            QTimer.singleShot(0, lambda: self.close())

    @handling_error
    def _on_open_game(self, checked=None):
        path, mask = select_game_file(self)
        if path:
            dialog = NewGameDialog(self.settings, self.game, self.share_dir, show_exit=False, open_file=(path,mask), parent=self)
            result = dialog.exec_()

            if result == QDialog.Accepted:
                self._new_game(dialog)

            if self.splashscreen:
                self.splashscreen.finish(self)

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
        try:
            prev_board = self.game.undo()
            self.board.fields_setup(prev_board)
            self.board.repaint()
            self.history.fill()
        except RequestError as e:
            error = e.rs.json().get("error", None)
            if error == "NothingToUndo":
                logging.warning(_("Nothing to undo."))
            else:
                raise e

    @handling_error
    def _on_draw_rq(self, checked=None):
        messages = self.game.request_draw()
        for message in messages:
            self.board.process_message(message)
        self.request_draw_action.setEnabled(False)
        self.capitulate_action.setEnabled(False)

    @handling_error
    def _on_accept_draw(self, checked=None):
        messages = self.game.accept_draw(True)
        for message in messages:
            self.board.process_message(message)

    @handling_error
    def _on_decline_draw(self, checked=None):
        messages = self.game.accept_draw(False)
        for message in messages:
            self.board.process_message(message)

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
            result_text = _("Game result: {}").format(result)
            self.status_info.setText(result_text)
            self.board.setCursor(Qt.ArrowCursor)
            game_over = _("Game over.")
            self.statusBar().showMessage(game_over)
            self.board.text_message = game_over + " " + result_text
            self.history.fill()
            self.request_draw_action.setEnabled(False)
            self.capitulate_action.setEnabled(False)
        elif isinstance(message, OtherSideMove):
            self.message.setText(str(message))
            self.history.fill()
            self.my_turn = True
        elif isinstance(message, WaitingMove):
            text = str(message)
            self.statusBar().showMessage(text)
            self.board.text_message = text
        elif isinstance(message, DrawRequestedMessage):
            ok = QMessageBox.question(self, _("Accept the draw?"),
                    _("Another side have offered you a draw. Do you wish to accept it?"))
            if ok == QMessageBox.Yes:
                self._on_accept_draw()
            else:
                self._on_decline_draw()
        elif isinstance(message, DrawResponseMessage):
            text = str(message)
            self.message.setText(text)
            self.board.text_message = text
            self.board.repaint()
            if not message.result:
                self.request_draw_action.setEnabled(True)
                self.capitulate_action.setEnabled(True)

    def _on_server_log(self, level, message):
        item = QListWidgetItem(self.log)
        item.setText(message)
        icon = None
        if level == "DEBUG":
            icon = QIcon.fromTheme("document-properties")
        elif level == "INFO":
            icon = QIcon.fromTheme("dialog-information")
        elif level == "ERROR":
            icon = QIcon.fromTheme("dialog-error")
        elif level == "WARNING":
            icon = QIcon.fromTheme("dialog-warning")
        if icon is not None:
            item.setIcon(icon)
        self.log.update()
        self.log.scrollToBottom()

    def _log_context_menu(self):
        menu = QMenu(self)
        menu.addAction(self.clear_log_action)
        menu.addAction(self.copy_log_action)
        menu.addAction(self.save_log_action)
        return menu

    def _on_log_context_menu(self, pos):
        menu = self._log_context_menu()
        menu.exec_(self.log.mapToGlobal(pos))

    def _on_clear_log(self, checked):
        self.log.clear()
        logging.info(_("Log has been cleared."))

    def _on_save_log(self, checked):
        text = ""
        for row in range(self.log.count()):
            text = text + self.log.item(row).text() + "\n"
        (path,mask) = QFileDialog.getSaveFileName(self, _("Save file"), ".", LOG_MASK)
        if path:
            with open(path, 'w') as f:
                f.write(text)#.encode("utf-8"))

    def _on_copy_log(self, checked):
        items = self.log.selectedItems()
        if not items:
            return
        text = items[0].text()
        QApplication.clipboard().setText(text)

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
            self.board.theme.enable_sound = dialog.get_enable_sound()
            self.settings.sync()
            logging.info(_("Settings have been updated."))

    def _handle_game_error(self, rs):
        try:
            json = rs.json()
        except:
            json = None

        message_format = _("Unexpected response received from the server.\nRequest URL: {}\nResponse code: {}\nResponse message: {}")
        if json is None:
            message = message_format.format(rs.url, rs.status_code, rs.text)
        else:
            err_msg = json.get("error", "Unspecified")
            if err_msg == "no such move":
                move = json.get("move", "?")
                #board = Game.parse_board(json["board"])
                #board = self.board
                #possible = json.get("possible", [])
                #possible = [board.show_move(Move.fromJson(m)) for m in possible]
                message = _("No such move: {}").format(move)
            elif err_msg == "invalid game status":
                expected = json.get("expected", "?")
                actual = json.get("actual", "?")
                message = _("Status of current game is unsuitable for this operation. Game status is {}; required status is {}").format(expected, actual)
            else:
                message = message_format.format(rs.url, rs.status_code, err_msg)

        QMessageBox.critical(self, _("Exception"), message)
        logging.exception(message)

    def _handle_connection_error(self, url, e):
        message = _("An exception occured while connecting to the server.\nRequest URL: {}\nException text: {}").format(url, e)
        QMessageBox.critical(self, _("Exception"), message)
        logging.exception(message)
        self._connection_failed = True

    def closeEvent(self, ev):
        
        if self.game.is_active():
            try:
                self.game.capitulate()
            except Exception as e:
                logging.exception(e)
                print(e)

        use_local_server = self.settings.value("use_local_server", type=bool)
        if use_local_server and self.local_server_used:
            try:
                self.game.shutdown()
            except RequestError as e:
                self._handle_game_error(e.rs)
            except Exception as e:
                logging.exception(e)
                print(e)

        self.settings.setValue("UI/geometry", self.saveGeometry())
        self.settings.setValue("UI/windowState", self.saveState())
        QMainWindow.closeEvent(self, ev)

    @handling_error
    def timerEvent(self, e):
        if e.timerId() != self.poll_timer:
            return

        if self._connection_failed:
            self._poll_try_number = self._poll_try_number + 1
            if self._poll_try_number < 10:
                return

        if self.setup_fields_on_poll and not self.game_active:
            state = self.game.get_state()
            if state["status"] == 'Running':
                self.game_active = True
                my_side = 'First' if self.game.user_side == FIRST else 'Second'
                self.my_turn = state['side'] == my_side
                #self.statusBar().clearMessage()

        if not self.game.is_active():
            return

        self._poll_try_number = 0

        board, messages = self.game.poll()
        for message in messages:
            self.board.process_message(message)
            if "move" in message:
                self.my_turn = True
        if self.setup_fields_on_poll:
            self.board.fields_setup(board)

