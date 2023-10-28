
from PyQt5.QtGui import QPainter, QPixmap, QIcon
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal
from PyQt5.QtWidgets import (QWidget, QDialog, QVBoxLayout, QHBoxLayout, QFormLayout,
            QLineEdit, QComboBox, QGroupBox, QCheckBox, QDialogButtonBox, QTableWidget,
            QTableWidgetItem, QAbstractItemView, QTableView, QToolBar)

from hcheckers.common import *
from hcheckers.game import Game

class HistoryTableWidget(QTableWidget):
    def __init__(self, client, board, parent=None):
        QTableWidget.__init__(self, parent)
        self.client = client
        self.board = board

        self.setColumnCount(2)
        self.setSelectionMode(QAbstractItemView.SingleSelection)

    def fill(self):
        self.clearContents()

        def make_item(value):
            item = QTableWidgetItem(value)
            item.setFlags(Qt.ItemIsEnabled | Qt.ItemIsSelectable)
            return item

        first, second = self.client.get_colors()
        self.setHorizontalHeaderLabels([first, second])

        history = self.client.get_history()
        if history is None:
            return
        self.setRowCount((len(history) // 2) + 2)
        row = 0
        self.setItem(row, 0, make_item(_("Initial position")))
        self.setItem(row, 1, make_item(_("Initial position")))
        self.setVerticalHeaderItem(0, QTableWidgetItem("-"))
        row = 1
        for record in reversed(history):
            side = record["side"]
            move = Move.fromJson(record["move"])
            if side == 'First':
                column = 0
            else:
                column = 1

            self.setItem(row, column, make_item(self.board.show_move(move)))
            if side == 'Second':
                self.setVerticalHeaderItem(row, QTableWidgetItem(str(row)))
                row = row + 1
        self.setVerticalHeaderItem(row, QTableWidgetItem("…"))

class HistoryDockerWidget(QWidget):
    def __init__(self, client, board, toplevel):
        QWidget.__init__(self, toplevel)
        self.toplevel = toplevel
        
        self.view_mode = False

        layout = QVBoxLayout()

        self.toolbar = QToolBar(self)
        layout.addWidget(self.toolbar)
        self.table = HistoryTableWidget(client, board,self)
        layout.addWidget(self.table)

        self.setLayout(layout)

        self._all_actions = []
        self.toggle_action = self._action(QIcon.fromTheme("go-jump"), _("Toggle history viewing mode"), self._on_toggle, key="Ctrl+Alt+H")
        self.toggle_action.setCheckable(True)

        self.first_action = self._action(QIcon.fromTheme("go-home"), _("Display initial position"), self._on_go_first, key="Ctrl+Home")
        self.next_action = self._action(QIcon.fromTheme("go-next"), _("Display next move"), self._on_go_next, key="Right")
        self.last_action = self._action(QIcon.fromTheme("go-last"), _("Display last move"), self._on_go_last, key="Ctrl+End")

        self._enable_history_actions()

        self._cell_callback_active = True
        self.table.cellActivated.connect(self._on_cell_changed)

    def _action(self, icon, text, callback, key=None):
        action = self.toolbar.addAction(icon, text)
        action.triggered.connect(callback)
        if key is not None:
            action.setShortcut(key)
        self._all_actions.append(action)
        return action
    
    def _history_actions(self):
        return [self.first_action, self.next_action, self.last_action]
    
    def _enable_history_actions(self):
        for action in self._history_actions():
            action.setEnabled(self.view_mode)

    def add_actions_to_menu(self, menu):
        for action in self._all_actions:
            menu.addAction(action)

    view_mode_toggled = pyqtSignal(bool)
    view_board = pyqtSignal(int, int)

    def fill(self):
        self.table.fill()

    def _on_toggle(self, checked):
        self.view_mode = checked
        self._enable_history_actions()
        self.view_mode_toggled.emit(checked)

    def _view_cell(self, row, column, select=False):
        item = self.table.item(row, column)
        if item:
            if select:
                self._cell_callback_active = False
                self.table.setCurrentCell(row, column)
                self._cell_callback_active = True
            self.view_board.emit(row-1, column+1)

    def _on_cell_changed(self, row, column):
        if self._cell_callback_active:
            if self.view_mode:
                self._view_cell(row, column)

    def _on_go_first(self, checked=None):
        self._view_cell(0, 0, select=True)

    def _on_go_last(self, checked=None):
        n_rows = self.table.rowCount()
        row = n_rows - 1
        col = 1
        item = None
        while item is None:
            if col == 0:
                col = 1
                row -= 1
            else:
                col = 0
            item = self.table.item(row, col)
        self._view_cell(row, col, select=True)

    def _on_go_next(self, checked=None):
        row = self.table.currentRow()
        col = self.table.currentColumn()
        n_rows = self.table.rowCount()

        if col == 0:
            col = 1
        else:
            col = 0
            row += 1

        if row < n_rows:
            self._view_cell(row, col, select=True)

