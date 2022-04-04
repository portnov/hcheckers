
from PyQt5.QtGui import QPainter, QPixmap
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

    def fill(self):
        self.clearContents()

        def make_item(value):
            item = QTableWidgetItem(value)
            item.setFlags(Qt.ItemIsEnabled | Qt.ItemIsSelectable)
            return item

        first, second = self.client.get_colors(self.client.rules)
        self.setHorizontalHeaderLabels([first, second])

        history = self.client.get_history()
        if history is None:
            return
        self.setRowCount((len(history) / 2) + 2)
        row = 0
        self.setItem(row, 0, make_item(_("Initial position")))
        self.setItem(row, 1, make_item(_("Initial position")))
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
                row = row + 1

class HistoryDockerWidget(QWidget):
    def __init__(self, client, board, parent=None):
        QWidget.__init__(self, parent)
        
        self.view_mode = False

        layout = QVBoxLayout()

        self.toolbar = QToolBar(self)
        layout.addWidget(self.toolbar)
        self.table = HistoryTableWidget(client, board,self)
        layout.addWidget(self.table)

        self.setLayout(layout)

        self.toggle_action = self.toolbar.addAction("Toggle")
        self.toggle_action.setCheckable(True)
        self.toggle_action.triggered.connect(self._on_toggle)

        self.table.cellActivated.connect(self._on_cell_changed)

    view_mode_toggled = pyqtSignal(bool)
    view_board = pyqtSignal(int, int)

    def fill(self):
        self.table.fill()

    def _on_toggle(self, checked):
        self.view_mode = checked
        self.view_mode_toggled.emit(checked)

    def _on_cell_changed(self, row, column):
        if self.view_mode:
            self.view_board.emit(row-1, column+1)

