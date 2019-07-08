
from requests.exceptions import ConnectionError

from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QWidget, QDialog, QVBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, QDialogButtonBox, QTableWidget, QTableWidgetItem, QAbstractItemView, QTableView

from hcheckers.common import *
from hcheckers.game import Game

class SelectedGame(object):
    def __init__(self):
        self.id = None
        self.rules = None
        self.first = None
        self.second = None
        self.status = None

    def get_used_name(self):
        if self.first is not None:
            return self.first
        if self.second is not None:
            return self.second
        return None

class LobbyWidget(QWidget):
    def __init__(self, client, rules=None, parent=None):
        QWidget.__init__(self, parent)
        self.rules = rules
        self.client = client
        self.dialog = parent

        layout = QVBoxLayout()

        self.table = QTableWidget(self)
        self.table.setColumnCount(5)
        self.table.setHorizontalHeaderLabels([_("ID"), _("Rules"), _("White"), _("Black"), _("Status")])
        self.table.currentCellChanged.connect(self._on_select)

        layout.addWidget(self.table)

        self.setLayout(layout)

        #self.fill()

    selected = pyqtSignal(object)

    def set_selectable(self, new_only):
        def set(row, col, selectable):
            flags = self.table.item(row, col).flags()
            self.table.item(row, col).setFlags(flags | selectable)

        for row in range(self.table.rowCount()):
            status = self.table.item(row, 4).text()
            selectable = (not new_only) or status == "New"
            set(row, 0, selectable)
            set(row, 1, selectable)
            set(row, 2, selectable)
            set(row, 3, selectable)
            set(row, 4, selectable)

    def fill(self, new_only=True):
        def make_item(game, key, selectable=True):
            value = game[key]
            item = QTableWidgetItem(value)
            flags = Qt.ItemIsEnabled
            if selectable:
                flags = flags | Qt.ItemIsSelectable
            item.setFlags(flags)
            return item

        def fill_row(row, game):
            selectable = (not new_only) or game["status"] == "New"
            self.table.setItem(row, 0, make_item(game, "id", selectable))
            self.table.setItem(row, 1, make_item(game, "rules", selectable))
            self.table.setItem(row, 2, make_item(game, "first", selectable))
            self.table.setItem(row, 3, make_item(game, "second", selectable))
            self.table.setItem(row, 4, make_item(game, "status", selectable))

        try:
            games = self.client.get_games(self.rules)
            self.table.setRowCount(len(games))
            for row, game in enumerate(games):
                fill_row(row, game)
            self.dialog.get_ok_button().setEnabled(True)
            self.dialog.message(None)
        except ConnectionError as e:
            self.dialog.message("Cannot connect to server")
            self.dialog.get_ok_button().setEnabled(False)
            self.dialog.refresh_button.setVisible(True)

        self.table.setSelectionMode(QTableView.SingleSelection)
        self.table.setSelectionBehavior(QTableView.SelectRows)
        self.table.resizeColumnsToContents()

    def _on_select(self, row, col, prev_row, prev_col):
        if row != prev_row:
            game = SelectedGame()
            game.id = self.table.item(row, 0).text()
            game.rules = self.table.item(row, 1).text()
            game.first = self.table.item(row, 2).text()
            game.second = self.table.item(row, 3).text()
            game.status = self.table.item(row, 4).text()
            self.selected.emit(game)

    def get_game_id(self):
        row = self.table.currentRow()
        if row is None:
            return None
        item = self.table.item(row, 0)
        if not item:
            return None
        game_id = item.text()
        return game_id
    
    def get_rules(self):
        row = self.table.currentRow()
        if row is None:
            return None
        rules = self.table.item(row, 1).text()
        return rules
    
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
    
    def get_used_name(self):
        row = self.table.currentRow()
        if row is None:
            return None

        user = self.table.item(row, 2).text()
        if user:
            return user
        user = self.table.item(row, 3).text()
        if user:
            return user
        return None

