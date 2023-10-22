
import sys
from os.path import join, exists, dirname
import os
import re
import logging
import subprocess
import shlex
import time
import webbrowser

from PyQt5.QtGui import QPainter, QPixmap, QIcon
from PyQt5.Qt import QStyle
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import (
        QApplication, QWidget, QToolBar, QMainWindow, QDialog,
        QVBoxLayout, QAction, QActionGroup, QLabel, QFileDialog,
        QFrame, QDockWidget, QMessageBox, QListWidget, QListWidgetItem,
        QMenu, QSplashScreen, QPushButton
    )

from hcheckers.field import Field
from hcheckers.common import *
from hcheckers.game import Game, AI, RequestError
from hcheckers.board import Board
from hcheckers.theme import Theme
from hcheckers.history import HistoryDockerWidget
from hcheckers.newgamedlg import *
from hcheckers.settingsdlg import SettingsDialog
from hcheckers.logutils import *

WAITING_MOVE_MESSAGE_DELAY = 3 # in seconds

class CountsWidget(QWidget):
    def __init__(self, parent):
        QWidget.__init__(self, parent)
        self.toplevel = parent
        layout = QHBoxLayout()

        self.first_men_icon, self.first_men = self._label(layout, MAN, FIRST)
        self.first_kings_icon, self.first_kings = self._label(layout, KING, FIRST)
        self.second_men_icon, self.second_men = self._label(layout, MAN, SECOND)
        self.second_kings_icon, self.second_kings = self._label(layout, KING, SECOND)

        self.setLayout(layout)

    def _get_piece(self, kind, side):
        icon_size = self.style().pixelMetric(QStyle.PM_ToolBarIconSize)
        inverted = self.toplevel.board.invert_colors
        pixmap = self.toplevel.board.theme.get_piece(Piece(kind, side), size=icon_size, invert=inverted)
        return pixmap

    def _label(self, layout, kind, side):
        pixmap = self._get_piece(kind, side)
        icon_label = QLabel(self)
        icon_label.setPixmap(pixmap)

        text_label = QLabel("0", self)
        layout.addWidget(icon_label)
        layout.addWidget(text_label)

        return icon_label, text_label

    def update_icons(self):
        self.first_men_icon.setPixmap(self._get_piece(MAN, FIRST))
        self.first_kings_icon.setPixmap(self._get_piece(KING, FIRST))
        self.second_men_icon.setPixmap(self._get_piece(MAN, SECOND))
        self.second_kings_icon.setPixmap(self._get_piece(KING, SECOND))

    def set(self, first_men, first_kings, second_men, second_kings):
        self.first_men.setText(str(first_men))
        self.first_kings.setText(str(first_kings))
        self.second_men.setText(str(second_men))
        self.second_kings.setText(str(second_kings))
        first, second = self.toplevel.game.get_colors()
        self.setToolTip(_("{}: {} men, {} kings\n{}: {} men, {} kings").format(first, first_men, first_kings, second, second_men, second_kings))

class LabelWithIcon(QWidget):
    clicked = pyqtSignal()

    def __init__(self, parent):
        QWidget.__init__(self, parent)
        layout = QHBoxLayout()
        self.text_label = QLabel(self)
        self.icon_label = QLabel(self)
        layout.addWidget(self.text_label, 1)
        layout.addWidget(self.icon_label)
        self.setLayout(layout)

    def mousePressEvent(self, ev):
        self.clicked.emit()

    def setIcon(self, icon):
        icon_size = self.style().pixelMetric(QStyle.PM_TabBarIconSize)
        self.icon_label.setPixmap(icon.pixmap(icon_size))

    def setText(self, text):
        self.text_label.setText(text)

    def setToolTip(self, text):
        self.text_label.setToolTip(text)
        self.icon_label.setToolTip(text)

class Checkers(QMainWindow):
    def __init__(self, share_dir):
        QMainWindow.__init__(self)
        self.share_dir = share_dir
        self.setWindowTitle(_("HCheckers client"))
        self.setWindowIcon(self._icon("hcheckers.svg"))
        self.resize(1024, 1024)
        self.settings = QSettings("hcheckers", "hcheckers")
        self._board_setup_mode = False
        self._game_active = False
        self._connection_failed = False
        self._poll_try_number = 0
        self._ai_session = None
        self._waiting_ai_hint = False
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

    def _enable_action(self, action, enable):
        action.setEnabled(enable)
        #w = self.toolbar.widgetForAction(action)
        #if w:
        #    print("W", w, enable)
        #    w.setVisible(enable)

    def set_board_setup_mode(self,mode):
        self._board_setup_mode = mode
        self._enable_action(self.run_action, mode)
        self._enable_action(self.put_first_action, mode)
        self._enable_action(self.put_second_action, mode)
        self._enable_action(self.erase_action, mode)

    board_setup_mode = property(get_board_setup_mode, set_board_setup_mode)

    def get_my_turn(self):
        return self.board.my_turn

    def set_my_turn(self, value):
        self.board.my_turn = value
        if value:
            self.statusBar().showMessage(_("Your turn."))
            self.board.hide_text_message()
            #self.board.repaint()
        else:
            self.statusBar().showMessage(_("Awaiting a turn from another side."))
        self._ai_session_dependencies()

    my_turn = property(get_my_turn, set_my_turn)

    def get_game_active(self):
        return self._game_active

    def set_game_active(self, value):
        self._game_active = value
        self.board.locked = not value

    game_active = property(get_game_active, set_game_active)

    def get_ai_session(self):
        return self._ai_session

    def set_ai_session(self, value):
        self._ai_session = value
        self._ai_session_dependencies()
    
    ai_session = property(get_ai_session, set_ai_session)

    def _waiting_draw_response(self):
        return self.game.draw_state == WE_REQUESTED_DRAW

    def _ai_session_dependencies(self):
        is_waiting = self._waiting_ai_hint or self._waiting_draw_response()
        stop_ai_enabled = (is_waiting or not self.my_turn) and self._ai_session is not None
        self.stop_ai_action.setEnabled(self.game_active and stop_ai_enabled)
        self.ai_hint_action.setEnabled(self.game_active and not stop_ai_enabled)
        self._enable_game_control_actions(self.game_active and self.my_turn and not is_waiting)
        self._enable_file_actions((self.my_turn or not self.game_active) and not is_waiting)

    def _start_server(self):
        self.proxy_usage = self.settings.value("proxy_usage", type=int)
        self.proxy_address = self.settings.value("proxy_address", EXAMPLE_PROXY)

        server_running = Game.check_server(self.server_url, Game.get_proxies_static(self.proxy_usage, self.proxy_address))
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
                message = _("Running local server: {}").format(server_path)
                print(message)
                logging.info(message)
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
        self.theme.enable_sound = self.settings.value("enable_sound", True, type=bool)
        self.game = Game(url = self.server_url, proxy_usage=self.proxy_usage, proxy_address=self.proxy_address)
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
        self.board.on_fields_setup.connect(self._on_board_update)
        self.board.field_clicked.connect(self._on_field_clicked)
        #self.board.show()
        self.toolbar = QToolBar(self)

        topbox = QHBoxLayout()
        self.message = QLabel(self)
        topbox.addWidget(self.message)
        self.count_status = CountsWidget(self)
        self.board.on_theme_changed.connect(self.count_status.update_icons)
        topbox.addStretch()
        topbox.addWidget(self.count_status)

        layout.addWidget(self.toolbar)
        layout.addLayout(topbox)
        layout.addWidget(self.board, stretch=1)
        widget.setLayout(layout)
        self.setCentralWidget(widget)

        self.status_info = QLabel(self)
        self.statusBar().addPermanentWidget(self.status_info)
        self.rules_info = QPushButton(self)
        #self.rules_info.setFrameStyle(QFrame.Sunken | QFrame.Panel)
        self.rules_info.setToolTip(_("Click to open the description of selected rules"))
        self.rules_info.setIcon(QIcon.fromTheme("help-contents"))
        self.rules_info.clicked.connect(self._on_rules_help)
        #self.rules_info.setLineWidth(3)
        self.statusBar().addPermanentWidget(self.rules_info)
        self.opponent_info = QLabel(self)
        self.opponent_info.setFrameStyle(QFrame.Sunken | QFrame.Panel)
        #self.opponent_info.setLineWidth(3)
        self.statusBar().addPermanentWidget(self.opponent_info)

        self.history = HistoryDockerWidget(self.game, self.board, self)
        self.history.view_mode_toggled.connect(self._on_history_view_toggle)
        self.history.view_board.connect(self._on_history_view_board)
        self.history_dock = QDockWidget(_("History"), self)
        self.history_dock.setAllowedAreas(Qt.AllDockWidgetAreas)
        self.history_dock.setWidget(self.history)
        self.history_dock.setObjectName("history")
        self.addDockWidget(Qt.RightDockWidgetArea, self.history_dock)
        self.history_dock.hide()

        self.log = QListWidget(self)
        self.log.setContextMenuPolicy(Qt.CustomContextMenu)
        self.log.customContextMenuRequested.connect(self._on_log_context_menu)
        self.log_dock = QDockWidget(_("Log"), self)
        self.log_dock.setAllowedAreas(Qt.AllDockWidgetAreas)
        self.log_dock.setWidget(self.log)
        self.log_dock.setObjectName("log")
        self.addDockWidget(Qt.BottomDockWidgetArea, self.log_dock)
        self.log_dock.hide()

        console_handler = logging.getLogger().handlers[0]
        logging.getLogger().removeHandler(console_handler)
        log_handler = UiLogHandler(self.log)
        level = self.settings.value("log_level", logging.INFO, type=int)
        logging.getLogger().setLevel(level)
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
        self.new_game_action = self._create_action(QIcon.fromTheme("document-new"), _("&New Game"), menu, self._on_new_game, key="Ctrl+N")
        self.open_game_action = self._create_action(QIcon.fromTheme("document-open"), _("&Open Game..."), menu, self._on_open_game, key="Ctrl+O")
        self.save_game_action = self._create_action(QIcon.fromTheme("document-save"), _("&Save Position"), menu, self._on_save_game, key="Ctrl+S")
        self.save_image_action = self._create_action(QIcon.fromTheme("document-save"), _("Save &Chart as image..."), menu, self._on_save_image, toolbar=False)
        self.undo_action = self._create_action(QIcon.fromTheme("edit-undo"), _("&Undo"), menu, self._on_undo, key="Ctrl+Z")

        menu.addSeparator()
        self.toolbar.addSeparator()

        self.stop_ai_action = self._create_action(QIcon.fromTheme("process-stop"), _("Ask AI to stop thinking"), menu, self.stop_ai, key="Ctrl+.")
        self.stop_ai_action.setEnabled(False)
        self.ai_hint_action = self._create_action(QIcon.fromTheme("dialog-information"), _("Ask for AI advice"), menu, self._on_ai_hint, key="Ctrl+A")

        menu.addSeparator()
        self.toolbar.addSeparator()

        self.request_draw_action = self._create_action(self._icon("draw_offer.svg"), _("Offer a &draw"), menu, self._on_draw_rq, key="Ctrl+D")
        self.capitulate_action = self._create_action(self._icon("handsup.svg"), _("Capitulate"), menu, self._on_capitulate, key="Ctrl+C")

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
        self.setup_actions = setup = QActionGroup(self)
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

        action = self.history_dock.toggleViewAction()
        action.setShortcut("Ctrl+H")
        menu.addAction(action)

        action = self.log_dock.toggleViewAction()
        action.setShortcut("Ctrl+L")
        menu.addAction(action)

        menu = self.menuBar().addMenu(_("H&istory"))
        self.history.add_actions_to_menu(menu)

        self.toolbar.addSeparator()
        menu = self.menuBar().addMenu(_("&Help"))
        self._create_action(QIcon.fromTheme("help-contents"), _("&Wiki documentation"), menu, self._on_help, key="F1")
        self._create_action(None, _("&About HCheckers"), menu, self._on_about, toolbar=False)

    def _game_control_actions(self):
        return  [self.undo_action, self.request_draw_action, self.capitulate_action, self.ai_hint_action]

    def _file_actions(self):
        return [self.new_game_action, self.open_game_action, self.save_game_action]

    def _enable_game_control_actions(self, enable):
        for action in self._game_control_actions():
            action.setEnabled(enable)

    def _enable_file_actions(self, enable):
        for action in self._file_actions():
            action.setEnabled(enable)

    @handling_error
    def _on_run_game(self, checked=None):
        self.board_setup_mode = False
        for action in self.setup_actions.actions():
            action.setChecked(False)
        board = self.board.json()
        self.game.start_new_game(self.game_settings.user_name, rules=self.game_settings.rules, user_turn_first=self.game_settings.user_turn_first, ai=self.game_settings.ai, board=board)
        self.board.hide_text_message()
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
        self.message.setText("")
        self.board.hide_text_message()
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
                self.game.start_new_game(game.user_name,
                            rules=game.rules,
                            user_turn_first=game.user_turn_first,
                            ai=game.ai,
                            fen_path=game.fen_path,
                            pdn_path=game.pdn_path,
                            previous_board_game=game.previous_board_game,
                            use_random_board_preset=game.use_random_board_preset)
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
            self.board.show_text_message(message)
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

        self._enable_game_control_actions(True)

        size, invert, notation, border_notation = self.game.get_notation(game.rules)
        self.board.invert_colors = invert
        self.board.topology = self.game.get_topology(game.rules)
        self.board.set_notation(size, notation, border_notation)

        self.board.theme = self.board.theme
        self.board.hint_moves = None
        self.board.invalidate()
        self.board.repaint()
        self.history.fill()
        self.count_status.update_icons()

    @handling_error
    def _on_new_game(self, checked=None, show_exit=False, open_file=(None,None)):
        dialog = NewGameDialog(self.settings, self.game, self.share_dir, show_exit, open_file=open_file, parent=self)
        result = dialog.exec_()

        if result == QDialog.Accepted:
            self._new_game(dialog)

        if self.splashscreen:
            self.splashscreen.finish(self)

        self._display_undo_count(None)
        self._display_hint_count(None)

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
    def _on_save_image(self, checked=None):
        (path,mask) = QFileDialog.getSaveFileName(self.board, _("Save file"), ".", "PNG images (*.png)")
        if path:
            self.board.save_image(path)

    def _display_undo_count(self, undo_count):
        undo_button = self.toolbar.widgetForAction(self.undo_action)
        if not undo_count:
            undo_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
            self.undo_action.setIconText(None)
            undo_button.setToolTip(_("Undo"))
        else:
            undo_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            self.undo_action.setIconText(str(undo_count))
            undo_button.setToolTip(_("Undo (number of previously undone moves: {})").format(undo_count))

    def _display_hint_count(self, hint_count):
        hint_button = self.toolbar.widgetForAction(self.ai_hint_action)
        if not hint_count:
            hint_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonIconOnly)
            self.ai_hint_action.setIconText(None)
            hint_button.setToolTip(_("Ask for AI advice"))
        else:
            hint_button.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
            self.ai_hint_action.setIconText(str(hint_count))
            hint_button.setToolTip(_("Ask for AI advice (number of previously requested advices: {})").format(hint_count))

    @handling_error
    def _on_undo(self, checked=None):
        try:
            prev_board = self.game.undo()
            self.board.fields_setup(prev_board)
            self.board.hint_moves = None
            self.board.repaint()
            self.history.fill()
            self._display_undo_count(self.game.undo_count)
        except RequestError as e:
            error = e.rs.json().get("error", None)
            if error == "NothingToUndo":
                logging.warning(_("Nothing to undo."))
            else:
                raise e

    @handling_error
    def stop_ai(self, checked=None):
        self.game.stop_ai(self.ai_session)
        self.ai_session = None
        self._waiting_ai_hint = False
    
    @handling_error
    def _on_ai_hint(self, checked=None):
        self._waiting_ai_hint = True
        self.ai_session = self.game.ai_hint()
        self.board.setCursor(Qt.WaitCursor)
        self.board.locked = True
        self.ai_hint_action.setEnabled(False)
        text = _("Waiting for an advice from AI")
        self.statusBar().showMessage(text)
        self.board.show_text_message(text, delay=WAITING_MOVE_MESSAGE_DELAY)
        self._display_hint_count(self.game.hint_count)

    @handling_error
    def _on_draw_rq(self, checked=None):
        ok = QMessageBox.question(self, _("Offer a draw?"),
                _("Are you sure you want to offer a draw to the other side? This action can not be undone."))
        if ok == QMessageBox.Yes:
            self.ai_session, messages = self.game.request_draw()
            for message in messages:
                self.board.process_message(message)
            self.request_draw_action.setEnabled(False)
            self.capitulate_action.setEnabled(False)
            self.ai_hint_action.setEnabled(False)
            self.board.locked = True
            self.board.setCursor(Qt.WaitCursor)
            text = _("Waiting for a response on draw offer from another side")
            self.statusBar().showMessage(text)
            self.board.show_text_message(text, delay=WAITING_MOVE_MESSAGE_DELAY)

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
        ok = QMessageBox.question(self, _("Capitulate?"),
                _("Are you sure you want to capitulate? This action can not be undone."))
        if ok == QMessageBox.Yes:
            messages = self.game.capitulate()
            for message in messages:
                self.board.process_message(message)
            self.board.invalidate()
            self.board.repaint()
            #self.my_turn = False

    @handling_error
    def get_result_str(self, result):
        first, second = self.game.get_colors()
        if result == 'FirstWin':
            return _("{} win").format(first)
        elif result == 'SecondWin':
            return _("{} win").format(second)
        else:
            return _("Draw")

    def _on_board_update(self):
        counts = self.board.piece_counts()
        self.count_status.set(*counts)

    def _on_board_message(self, message):
        if isinstance(message, GameResultMessage):
            self.game_active = False
            result = self.get_result_str(message.result)
            result_text = _("Game result: {}").format(result)
            self.status_info.setText(result_text)
            self.board.setCursor(Qt.ArrowCursor)
            game_over = _("Game over.")
            self.statusBar().showMessage(game_over)
            self.board.show_text_message(game_over + " " + result_text)
            self.history.fill()
            #self.request_draw_action.setEnabled(False)
            #self.capitulate_action.setEnabled(False)
            self.stop_ai_action.setEnabled(False)
            self.ai_hint_action.setEnabled(False)
            self._enable_game_control_actions(False)
            self.board.hint_moves = None
            self.board.invalidate()
            self.board.repaint()
        elif isinstance(message, OtherSideMove):
            self.message.setText(str(message))
            self.history.fill()
            self.my_turn = True
            self.board.hide_text_message()
            self.board.repaint()
        elif isinstance(message, WaitingMove):
            text = str(message)
            self.statusBar().showMessage(text)
            self.board.show_text_message(text, delay=WAITING_MOVE_MESSAGE_DELAY)
        elif isinstance(message, DrawRequestedMessage):
            ok = QMessageBox.question(self, _("Accept the draw?"),
                    _("Another side have offered you a draw. Do you wish to accept it?"))
            if ok == QMessageBox.Yes:
                self._on_accept_draw()
            else:
                self._on_decline_draw()
        elif isinstance(message, DrawResponseMessage):
            self.board.hide_text_message()
            text = str(message)
            self.message.setText(text)
            QMessageBox.information(self,
                    _("Response on a draw offer"),
                    text)
            #self.board.show_text_message(text)
            #self.board.repaint()
            self.stop_ai_action.setEnabled(False)
            self.request_draw_action.setEnabled(True)
            self.capitulate_action.setEnabled(True)
            self.ai_hint_action.setEnabled(True)
            self.game.draw_state = None
            self.board.locked = False
            self.board.setCursor(Qt.ArrowCursor)
            self.statusBar().showMessage(_("Your turn."))
            self.ai_session = None
            self.board.invalidate()
            self.board.repaint()
        elif isinstance(message, AiHintMessage):
            logging.info(_("AI suggested the following move(s): {}").format("; ".join([self.board.show_move(m) for m in message.moves])))
            self._waiting_ai_hint = False
            self.board.hint_moves = message.moves
            self.board.setCursor(Qt.ArrowCursor)
            self.board.locked = False
            self.board.hide_text_message()
            self.statusBar().showMessage(str(message))
            self.ai_hint_action.setEnabled(True)
            self.stop_ai_action.setEnabled(False)
            self.ai_session = None
            self.board.repaint()

    def _on_server_log(self, level, message):
        if level == "DEBUG":
            logging.debug(message)
        elif level == "INFO":
            logging.info(message)
        elif level == "WARNING":
            logging.warning(message)
        elif level == "ERROR":
            logging.error(message)

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
        dialog = SettingsDialog(self.settings, self.share_dir, parent=self, client=self.game)
        result = dialog.exec_()
        if result == QDialog.Accepted:
            self.board.show_possible_moves = dialog.get_show_possible_moves()
            self.board.show_notation = dialog.get_show_notation()
            self.board.show_border = dialog.get_show_border()
            self.board.theme = dialog.get_theme()
            self.board.theme.enable_sound = dialog.get_enable_sound()
            level = self.settings.value("log_level", logging.INFO, type=int)
            logging.getLogger().setLevel(level)
            self.settings.sync()
            logging.info(_("Settings have been updated."))

    @handling_error
    def _on_history_view_toggle(self, view_mode):
        self._enable_game_control_actions(not view_mode)
        self._enable_file_actions(not view_mode)
        self.board.locked = view_mode
        if view_mode:
            self.board.reset_moveable()
        if not view_mode:
            self.board.show_board(None)

    @handling_error
    def _on_history_view_board(self, turn_idx, side):
        if turn_idx < 0:
            next_board = self.game.get_initial_board()
        else:
            move, prev_board, next_board = self.game.get_move_with_result(turn_idx, side)
            self.board.show_board(prev_board)
            self.board.start_move_animation(move)

        self.board.show_board(next_board)
        if turn_idx < 0:
            self.board.reset_last_moved()

    def _on_help(self, checked=None):
        webbrowser.open(WIKI_URL)

    def _on_about(self, checked=None):
        server_version = self.game.get_version()
        title = _("About HCheckers")
        text = _("This is HCheckers Client application, version {0}.<br>Server version is {1} ({2}).<br>Please report issues at <a href=\"{3}\">{3}</a>.").format(HCHECKERS_VERSION, server_version['release'], server_version['hash'], BUGTRACKER_URL)
        QMessageBox.about(self, title, text)

    def _on_rules_help(self):
        open_rules_help(self.game.rules)

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
                message = _("Status of current game is unsuitable for this operation. Game status is {}; required status is {}").format(actual, expected)
            elif err_msg == "custom ai settings disabled":
                message = _("Custom AI settings are disabled on server side")
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
            if self.ai_session is not None:
                try:
                    self.stop_ai()
                except Exception as e:
                    logging.exception(e)
                    print(e)

            try:
                self.game.capitulate()
            except Exception as e:
                logging.exception(e)
                print(e)

        use_local_server = self.settings.value("use_local_server", True, type=bool)
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

