
from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QWidget, QDialog, QVBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, QDialogButtonBox, QTableWidget, QTableWidgetItem, QAbstractItemView, QTableView

from common import *
from game import Game

class LobbyWidget(QWidget):
    def __init__(self, client, rules=None, parent=None):
        QWidget.__init__(self, parent)
        self.rules = rules
        self.client = client

        layout = QVBoxLayout()

        self.table = QTableWidget(self)
        self.table.setColumnCount(5)
        self.table.setHorizontalHeaderLabels([_("ID"), _("Rules"), _("White"), _("Black"), _("Status")])

        layout.addWidget(self.table)

        self.setLayout(layout)

        self.fill()

    def fill(self):
        def make_item(game, key, selectable=True):
            value = game[key]
            item = QTableWidgetItem(value)
            flags = Qt.ItemIsEnabled
            if selectable:
                flags = flags | Qt.ItemIsSelectable
            item.setFlags(flags)
            return item

        def fill_row(row, game):
            selectable = game["status"] == "New"
            self.table.setItem(row, 0, make_item(game, "id", selectable))
            self.table.setItem(row, 1, make_item(game, "rules", selectable))
            self.table.setItem(row, 2, make_item(game, "first", selectable))
            self.table.setItem(row, 3, make_item(game, "second", selectable))
            self.table.setItem(row, 4, make_item(game, "status", selectable))

        games = self.client.get_games(self.rules)
        self.table.setRowCount(len(games))
        for row, game in enumerate(games):
            fill_row(row, game)

        self.table.setSelectionMode(QTableView.SingleSelection)
        self.table.setSelectionBehavior(QTableView.SelectRows)
        self.table.resizeColumnsToContents()

    def get_game_id(self):
        row = self.table.currentRow()
        if row is None:
            return None
        game_id = self.table.item(row, 0).text()
        return game_id
    
    def get_free_side(self):
        row = self.table.currentRow()
        if row is None:
            return None

        user = self.table.item(row, 2).text()
        if not user:
            return FIRST
        user = self.table.item(row, 3).text()
        if not user:
            return SECOND
        return None

