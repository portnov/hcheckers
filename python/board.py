
from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, pyqtSignal
from PyQt5.QtWidgets import QApplication, QWidget

from field import Field
from game import Game

class Board(QWidget):
    def __init__(self, theme, game, parent=None):
        QWidget.__init__(self, parent)

        self.game = game
        self._theme = theme

        self.fields = {}
        self.field_by_label = {}
        for row in range(8):
            for col in range(8):
                letter = "abcdefgh"[col]
                digit = row + 1
                field = Field()
                field.theme = self._theme
                field.label = letter + str(digit)
                self.fields[(row, col)] = field
                self.field_by_label[field.label] = field

        self._pixmap = None

        self._selected_field = None
        self._valid_target_fields = None
        self._board = dict()

        self.setup_patterns()

    field_clicked = pyqtSignal(int, int)

    def get_theme(self):
        return self._theme

    def setup_patterns(self):
        for row in range(8):
            for col in range(8):
                if (row % 2) == (col % 2):
                    self.fields[(row,col)].pattern_id = 2
                else:
                    self.fields[(row,col)].pattern_id = 1

    def set_theme(self, theme):
        self._theme = theme
        field_size = self.get_target_field_size(self.size())
        self._theme.set_target_size(field_size)
        for field in self.fields.values():
            field.theme = theme

        self.fields_setup()

        self.repaint()

    theme = property(get_theme, set_theme)

    def get_selected_field(self):
        return self._selected_field

    def set_selected_field(self, value):
        self._selected_field = value

        for idx in self.fields:
            self.fields[idx].show_frame = (idx == value)

        self.invalidate()
        self.repaint()

    selected_field = property(get_selected_field, set_selected_field)

    def reset(self):
        self.fields_setup()

    def invalidate(self):
        for field in self.fields.values():
            field.invalidate()
        self._pixmap = None

    def fields_setup(self, board=None):
        if board is None:
            board = self.game.get_board()

        self._board = board

        for label in self.field_by_label:
            field = self.field_by_label[label]
            if label in board:
                field.piece = board[label]
            else:
                field.piece = None

        self.invalidate()

    def draw_field(self, painter, field, row, col):
        width = self.size().width()
        height = self.size().height()

        row_height = height / 8
        col_width = width / 8
        size = min(row_height, col_width)

        field.draw(painter, QRect(col * size, (7-row) * size, size, size))

    def draw(self):
        if self._pixmap is not None:
            return

        pixmap = QPixmap(self.size())
        pixmap.fill(Qt.white)
        painter = QPainter(pixmap)

        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            self.draw_field(painter, field, row, col)

        painter.end()

        self._pixmap = pixmap
        self.update()

    def get_target_field_size(self, size):
        w_max = size.width()
        h_max = size.height()
        max_size = min(w_max, h_max)

        return (max_size) / 8
    
    def paintEvent(self, e):
        self.draw()
        painter = QPainter(self)
        painter.drawPixmap(0, 0, self._pixmap)
        painter.end()

    def sizeHint(self):
        if self._theme is None:
            return QSize()
        else:
            width  = self._theme.get_field_size()
            height = self._theme.get_field_size()
            return QSize(width, height)
    
    def resizeEvent(self, e):
        if self._theme is not None:
            size = self.get_target_field_size(e.size())
            self._theme.set_target_size(size)
            self.fields_setup()

    def process_click(self, row, col):
        self.field_clicked.emit(row, col)

        field = self.fields[(row, col)]
        piece = self._board.get(field.label)
        if piece is not None and piece.side == self.game.user_side:
            moves = self.game.get_possible_moves(field.label)
            print(moves)
            if not moves:
                print("Piece at {} does not have moves".format(field.label))
            else:
                self._valid_target_fields = [move["steps"][-1]["field"] for move in moves]
                print("Valid target fields: {}".format(self._valid_target_fields))
                self.selected_field = (row, col)
        elif piece is None and self.selected_field is not None and self._valid_target_fields is not None:
            if field.label in self._valid_target_fields:
                src_field = self.fields[self.selected_field].label
                dst_field = field.label
                board, messages = self.game.move(src_field, dst_field)
                for message in messages:
                    print("Other side move: {}".format(message["move"]))
                    self._board = Game.parse_board(message["board"])
                self.selected_field = None
                self._valid_target_fields = None
                self.fields_setup(self._board)
                self.repaint()
        else:
            print("Field {} is not yours".format(field.label))


    def mousePressEvent(self, me):
        if me.button() != Qt.LeftButton:
            return
        
        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            if field.rect().contains(me.pos()):
                self.invalidate()
                self.update()
                self.process_click(row, col)
                return




