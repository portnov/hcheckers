
from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal
from PyQt5.QtWidgets import QWidget, QDialog, QVBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, QDialogButtonBox, QTableWidget, QTableWidgetItem, QAbstractItemView, QTableView

from common import *
from game import Game

class HistoryWidget(QTableWidget):
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
        self.setRowCount((len(history) / 2) + 1)
        row = 0
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


