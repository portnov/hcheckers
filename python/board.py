
import math
from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal
from PyQt5.QtWidgets import QApplication, QWidget

from field import Field
from game import Game, RequestError

class MoveAnimation(QObject):
    def __init__(self, parent):
        QObject.__init__(self, parent)
        self.board = parent
        self.move = None
        self.timer = parent.startTimer(50)
        self.steps = 10
        self.step = 0
        self.piece_position = None
        self.piece = None
        self.start_field = None
        self.end_field = None
        self.process_result = False

    finished = pyqtSignal(bool)

    def start(self, start_field, end_field, move, start_position, piece, process_result = True):
        if piece is None:
            raise Exception("piece is None")
        self.piece = piece
        self.piece_position = start_position
        self.step = 0
        self.steps = 10 * len(move["steps"])
        self.move = move
        self.captured_fields = [step["field"] for step in move["steps"] if step["capture"] == True]
        self.start_field = start_field
        self.end_field = end_field
        self.process_result = process_result

    def get_animated_step(self, progress):
        n = len(self.move["steps"])
        idx = int(math.floor(progress * n))
        if idx >= n:
            return None
        elif idx == 0:
            field_from = self.move["from"]
            field_to = self.move["steps"][0]["field"]
            return field_from, field_to
        else:
            field_from = self.move["steps"][idx-1]["field"]
            field_to   = self.move["steps"][idx]["field"]
            return field_from, field_to

    def interpolate(self, progress, p1, p2):
        x1,y1 = p1
        x2,y2 = p2
        x = (1-progress)*x1 + progress*x2
        y = (1-progress)*y1 + progress*y2
        #print("{} - {} @ {} = {}".format(p1, p2, progress, (x,y)))
        return (x,y)

    def update(self, time):
        step = self.get_animated_step(time)
        if step is not None:
            label_from, label_to = step
            idx_from, idx_to = self.board.index_by_label[label_from], self.board.index_by_label[label_to]
            center_from, center_to = self.board.get_field_center(idx_from), self.board.get_field_center(idx_to)
            self.piece_position = self.interpolate(time, center_from, center_to)
        else:
            self.piece_position = self.board.get_field_center(self.end_field)

    def stop(self):
        self.move = None
        self.step = 0
        self.piece_position = None
        self.piece = None
        self.finished.emit(self.process_result)
        self.process_result = False

    def is_active(self):
        return self.move is not None

    def progress(self):
        return float(self.step) / float(self.steps)

    def tick(self, e):
        if e.timerId() != self.timer:
            return False
        if not self.is_active():
            return False
        if self.step > self.steps:
            self.stop()
            return False
        self.step = self.step + 1
        self.update(self.progress())
        return True

class Board(QWidget):
    def __init__(self, theme, game, parent=None):
        QWidget.__init__(self, parent)

        self.game = game
        self._theme = theme
        self.n_fields = 8

        self.fields = {}
        self.field_by_label = {}
        self.index_by_label = {}
        for row in range(self.n_fields):
            for col in range(self.n_fields):
                letter = "abcdefgh"[col]
                digit = row + 1
                field = Field()
                field.theme = self._theme
                field.label = letter + str(digit)
                self.fields[(row, col)] = field
                self.field_by_label[field.label] = field
                self.index_by_label[field.label] = (row, col)

        self._pixmap = None

        self._selected_field = None
        self._valid_target_fields = None
        self._board = dict()
        self._new_board = None

        self.move_animation = MoveAnimation(self)
        self.move_animation.finished.connect(self.on_animation_finished)

        self.setup_patterns()

    field_clicked = pyqtSignal(int, int)

    def get_theme(self):
        return self._theme

    def setup_patterns(self):
        for row in range(self.n_fields):
            for col in range(self.n_fields):
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
        try:
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
        except RequestError as e:
            print(e)

    def draw_field(self, painter, field, row, col, hide=False):
        width = self.size().width()
        height = self.size().height()

        row_height = height / self.n_fields
        col_width = width / self.n_fields
        size = min(row_height, col_width)

        prev_hide_piece = field.hide_piece
        if hide:
            field.hide_piece = hide

        prev_possible_piece = field.possible_piece
        label = self.fields[(row,col)].label
        if self._selected_field is not None and self._valid_target_fields is not None and label in self._valid_target_fields:
            field.possible_piece = self.fields[self._selected_field].piece

        prev_captured = field.captured
        if self.move_animation.is_active() and label in self.move_animation.captured_fields:
            field.captured = True

        field.draw(painter, QRect(col * size, (self.n_fields-1-row) * size, size, size))
        if hide:
            field.hide_piece = prev_hide_piece
        field.possible_piece = prev_possible_piece
        field.captured = prev_captured

    def draw(self):
        if self._pixmap is not None:
            return

        pixmap = QPixmap(self.size())
        pixmap.fill(Qt.white)
        painter = QPainter(pixmap)

        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            hide = False
            if self.move_animation.is_active():
                hide = ((row, col) == self.move_animation.start_field) or ((row, col) == self.move_animation.end_field)
            self.draw_field(painter, field, row, col, hide=hide)

        if self.move_animation.is_active():
            piece = self.theme.get_piece(self.move_animation.piece)
            if piece is not None:
                w, h = piece.size().width(), piece.size().height()
                x0,y0 = self.move_animation.piece_position
                x = x0 - w*0.5
                y = y0 - h*0.5
                painter.drawPixmap(x, y, piece)

        painter.end()

        self._pixmap = pixmap
        self.update()

    def get_size(self):
        size = self.size()
        return min(size.width(), size.height())

    def get_target_field_size(self, size):
        w_max = size.width()
        h_max = size.height()
        max_size = min(w_max, h_max)

        return max_size / self.n_fields
    
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

    def get_field_center(self, idx):
        row, col = idx
        size = self.get_size()
        x = (col + 0.5) / self.n_fields
        y = (self.n_fields - 1 - row + 0.5) / self.n_fields
        #print("{} => {}".format(idx, (x,y)))
        return (x*size, y*size)

    def get_move_end_field(self, move):
        label = move["steps"][-1]["field"]
        return self.index_by_label[label]
    
    def timerEvent(self, e):
        if self.move_animation.tick(e):
            self._pixmap = None
            self.repaint()

    def process_click(self, row, col):
        self.field_clicked.emit(row, col)

        try:
            field = self.fields[(row, col)]
            piece = self._board.get(field.label)
            if piece is not None and piece.side == self.game.user_side:
                moves = self.game.get_possible_moves(field.label)
                print(moves)
                if not moves:
                    print("Piece at {} does not have moves".format(field.label))
                else:
                    self._valid_target_fields = dict((move["steps"][-1]["field"], move) for move in moves)
                    print("Valid target fields: {}".format(self._valid_target_fields.keys()))
                    self.selected_field = (row, col)
                    self.repaint()
            elif piece is None and self.selected_field is not None and self._valid_target_fields is not None:
                if field.label in self._valid_target_fields:
                    src_field = self.fields[self.selected_field].label
                    dst_field = field.label
                    self.game.begin_move(src_field, dst_field)

                    move = self._valid_target_fields[field.label]
                    start_position = self.get_field_center(self.index_by_label[src_field])
                    piece = self.fields[self.selected_field].piece
                    self.move_animation.start(self.selected_field, (row, col), move, start_position, piece, process_result=True)
                    self._valid_target_fields = None
                    self.repaint()

            else:
                print("Field {} is not yours".format(field.label))
        except RequestError as e:
            print(e)

    def on_animation_finished(self, process_result):
        if process_result:
            board, messages = self.game.get_move_result()
            for message in messages:
                move = message["move"]
                print("Other side move: {}".format(move))
                self._board = board
                self._new_board = Game.parse_board(message["board"])
                src_field = self.index_by_label[move["from"]]
                dst_field = self.get_move_end_field(move)
                start_position = self.get_field_center(src_field)
                piece = self.fields[src_field].piece
                self.move_animation.start(src_field, dst_field, move, start_position, piece, process_result = False)
            self.selected_field = None
        elif self._new_board is not None:
            self._board = self._new_board
            self._new_board = None
        self.fields_setup(self._board)
        self.repaint()

    def mousePressEvent(self, me):
        if me.button() != Qt.LeftButton:
            return
        
        if self.move_animation.is_active():
            return
        
        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            if field.rect().contains(me.pos()):
                self.invalidate()
                self.update()
                self.process_click(row, col)
                return

