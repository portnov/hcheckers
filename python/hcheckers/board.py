
import math
import logging
from collections import defaultdict

from PyQt5.QtGui import QPainter, QPainterPath, QPainterPathStroker, QPixmap,QPen, QBrush, QImage
from PyQt5.QtCore import QPointF, QRect, QRectF, QSize, Qt, QObject, QTimer, pyqtSignal
from PyQt5.QtWidgets import QApplication, QWidget

from hcheckers.common import *
from hcheckers.field import Field
from hcheckers.game import Game, RequestError

ANIMATION_STEP_DURATION = 50
ANIMATION_STEPS_PER_STEP = 10

BORDER_WIDTH_COEF = 0.75

class MoveAnimation(QObject):
    def __init__(self, parent):
        QObject.__init__(self, parent)
        self.board = parent
        self.move = None
        self.timer = parent.startTimer(ANIMATION_STEP_DURATION)
        self.steps = 10
        self.step = 0
        self.piece_position = None
        self.piece = None
        self.start_field = None
        self.end_field = None
        self.process_result = False
        self.captured_fields = []
        self._delayed_start = None
        self.started = False

    finished = pyqtSignal(bool)

    def start(self, start_field, end_field, move, start_position, piece, process_result = True):
        if piece is None:
            raise Exception("piece is None")

        if self.is_active():
            self._delayed_start = (start_field, end_field, move, start_position, piece, process_result)
            return

        self.move = move
        self.piece = piece
        self.piece_position = start_position
        self.step = 0
        self.steps = ANIMATION_STEPS_PER_STEP * len(move.steps)
        self.captured_fields = [step.field for step in move.steps if step.capture == True]
        self.start_field = start_field
        self.end_field = end_field
        self.process_result = process_result
        self.started = True
        self.play_sound()

    def play_sound(self):
        n_captured = len(self.captured_fields)
        steps = len(self.move.steps)
        if n_captured:
            self.board.theme.play_check(n_captured, ANIMATION_STEPS_PER_STEP * ANIMATION_STEP_DURATION)
        else:
            self.board.theme.play_move(steps, ANIMATION_STEPS_PER_STEP * ANIMATION_STEP_DURATION)

    def get_animated_step(self, progress):
        n = len(self.move.steps)
        idx = int(math.floor(progress * n))
        if idx >= n:
            return None
        elif idx == 0:
            field_from = self.move.from_field
            field_to = self.move.steps[0].field
            return field_from, field_to
        else:
            field_from = self.move.steps[idx-1].field
            field_to   = self.move.steps[idx].field
            return field_from, field_to

    def interpolate(self, progress, p1, p2):
        x1,y1 = p1
        x2,y2 = p2
        x = (1-progress)*x1 + progress*x2
        y = (1-progress)*y1 + progress*y2
        #logging.debug("{} - {} @ {} = {}".format(p1, p2, progress, (x,y)))
        return (x,y)

    def update(self, time):
        if time >= 1.0:
            time = 1.0
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
        self.started = False
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
            if self._delayed_start is None:
                return False
            else:
                self.start(*self._delayed_start)
                self._delayed_start = None
                return True
        self.step = self.step + 1
        self.update(self.progress())
        return True

class PossibleCaptureAnimation(QObject):
    def __init__(self, parent):
        QObject.__init__(self, parent)
        self.board = parent
        self.timer = parent.startTimer(ANIMATION_STEP_DURATION)
        self.steps = 10
        self.step = 0
        self.captured_fields = []
        self.attacking_fields = []
        self.started = False
        self.opacity = 0.0
        self.looping = False

    def start(self, moves, loop = False):
        self.captured_fields = sum([[step.field for step in move.steps if step.capture == True] for move in moves], [])
        self.attacking_fields = [move.from_field for move in moves if any(step.capture == True for step in move.steps)]
        self.step = 0
        self.steps = 2*ANIMATION_STEPS_PER_STEP
        self.started = True
        self.looping = loop

    def is_active(self):
        return len(self.captured_fields) > 0

    def progress(self):
        return 2 * float(self.step) / float(self.steps)

    def stop(self):
        self.captured_fields = []
        self.started = False
        self.opacity = 0.0
        self.step = 0

    def update(self, time):
        if time >= 2.0:
            time = 2.0
        if time <= 1.0:
            self.opacity = time
        else:
            self.opacity = 1.0 - (time - 1.0)

    def tick(self, e):
        if e.timerId() != self.timer:
            return False
        if not self.is_active():
            return False
        if self.step > self.steps:
            if self.looping:
                self.step = 0
            else:
                self.stop()
        self.step = self.step + 1
        self.update(self.progress())
        return True

    def draw_field(self, painter, field, sz, x, y):
        if not self.is_active():
            return
        is_captured = field.label in self.captured_fields
        is_attacking = field.label in self.attacking_fields
        if is_captured or is_attacking:
            painter.setOpacity(self.opacity)
            if is_captured:
                cross = self.board.theme.get_captured()
                painter.drawPixmap(sz.init_x + x, sz.init_y + y, cross)
            if is_attacking:
                attacking = self.board.theme.get_attacking()
                painter.drawPixmap(sz.init_x + x, sz.init_y + y, attacking)
            painter.setOpacity(1.0)

class TextMessage(QObject):
    sequence = 0

    def __init__(self, board, message, delay=None):
        QObject.__init__(self, board)
        TextMessage.sequence += 1
        self._sequence = TextMessage.sequence
        self.board = board
        self.message = message
        self.delay = delay
        self.actual = True
        self.show = delay is None
        if delay is not None:
            self.timer = QTimer.singleShot(delay*1000, self._show)
        else:
            self.timer = None

    def _show(self):
        if self.actual:
            self.show = True
            self.board.invalidate()
            self.board.repaint()

class SizeData():
    def __init__(self, init_x, init_y, cell_size):
        self.cell_size = cell_size
        self.border_init_x = self.init_x = init_x
        self.border_init_y = self.init_y = init_y
        self.border_width = None

class Board(QWidget):
    def __init__(self, theme, settings, game, toplevel):
        QWidget.__init__(self, toplevel)

        self.toplevel = toplevel
        self.game = game
        self._theme = theme
        self.settings = settings
        self._show_notation = settings.value("show_notation", True, type=bool)
        self._show_border = settings.value("show_border", False, type=bool)
        self.show_possible_moves = settings.value("show_possible_moves", True, type=bool)
        self.invert_colors = False
        self._flip = False
        self.topology = 'Diagonal'

        self.top_border_labels = None
        self.left_border_labels = None
        self.bottom_border_labels = None
        self.right_border_labels = None

        self._init_fields(8, 8)

        self._pixmap = None

        self._selected_field = None
        self._valid_target_fields = None
        self._moveable_fields = None
        self._last_left_field = None
        self._last_moved_field = None
        self._board = dict()
        self._new_board = None
        self._drawing_my_move = False
        self._text_message = None
        self._text_message_timer = None
        self._prev_hovered_field = None

        self._my_turn = True
        self.locked = False

        self.move_animation = MoveAnimation(self)
        self.move_animation.finished.connect(self.on_animation_finished)

        self.possible_captures_animation = PossibleCaptureAnimation(self)

        self.hint_moves = None

        self.setMouseTracking(True)
        self.setup_patterns()

    field_clicked = pyqtSignal(int, int)

    message = pyqtSignal(object)

    server_log = pyqtSignal(str, str)

    on_fields_setup = pyqtSignal()

    on_theme_changed = pyqtSignal()

    def _init_fields(self, rows, cols):
        self.n_rows = rows
        self.n_cols = cols
        self.fields = {}
        self.field_by_label = {}
        self.index_by_label = {}
        for row in range(self.n_rows):
            for col in range(self.n_cols):
                field = Field()
                field.theme = self._theme
                field.show_label = self._show_notation
                field.label = Label(col, row)
                self.fields[(row, col)] = field
                self.field_by_label[field.label] = field
                self.index_by_label[field.label] = (row, col)

    def get_my_turn(self):
        return self._my_turn

    def set_my_turn(self, value):
        self._my_turn = value
        if value:
            self.setCursor(Qt.ArrowCursor)
            moves = self.game.get_possible_moves()
            self._moveable_fields = set(move.from_field for move in moves)
            if self._highlight_captures() == SHOW_ALWAYS:
                self.possible_captures_animation.start(moves, loop=True)
        else:
            self.setCursor(Qt.WaitCursor)
            self._moveable_fields = None
            self.possible_captures_animation.stop()

    my_turn = property(get_my_turn, set_my_turn)

    def get_last_moved(self):
        return self._last_moved_field

    def set_last_moved(self, value):
        self._last_moved_field = value

        for idx in self.fields:
            self.fields[idx].last_moved = (idx == value)

        self.invalidate()
        #self.repaint()

    last_moved = property(get_last_moved, set_last_moved)

    def get_last_left(self):
        return self._last_left_field

    def set_last_left(self, value):
        self._last_left_field = value

        for idx in self.fields:
            self.fields[idx].last_left = (idx == value)

        self.invalidate()

    last_left = property(get_last_left, set_last_left)

    def reset_moveable(self):
        self._moveable_fields = None
        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            field.moveable = False
        self.invalidate()
        self.repaint()

    def reset_last_moved(self):
        self.last_moved = None
        self.last_left = None
        self.repaint()

    def get_theme(self):
        return self._theme

    def setup_patterns(self):
        all_usable = self.topology != 'Diagonal'
        for row in range(self.n_rows):
            for col in range(self.n_cols):
                if (row % 2) == (col % 2):
                    self.fields[(row,col)].pattern_id = 2
                    self.fields[(row,col)].usable = True
                else:
                    self.fields[(row,col)].pattern_id = 1
                    self.fields[(row,col)].usable = all_usable

    def set_theme(self, theme):
        self._theme = theme
        field_size = self.get_target_field_size(self.size())
        self._theme.set_target_size(field_size)
        for field in self.fields.values():
            field.theme = theme

        self.fields_setup()

        self.repaint()
        self.on_theme_changed.emit()

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

    def get_show_notation(self):
        return self._show_notation
    
    def set_show_notation(self, value):
        self._show_notation = value
        for idx in self.fields:
            self.fields[idx].show_label = value
        self.invalidate()
        self.repaint()

    show_notation = property(get_show_notation, set_show_notation)

    def get_show_border(self):
        return self._show_border

    def set_show_border(self, value):
        self._show_border = value
        self.invalidate()
        self.repaint()

    show_border = property(get_show_border, set_show_border)

    def get_flip(self):
        return self._flip

    def set_flip(self, value):
        self._flip = value
        self.invalidate()
        self.repaint()

    flip = property(get_flip, set_flip)

    def set_notation(self, size, pairs, border_notation):
        (rows, cols) = size
        self._init_fields(rows, cols)

        self.top_border_labels = border_notation["top"]
        self.left_border_labels = border_notation["left"]
        self.bottom_border_labels = border_notation["bottom"]
        self.right_border_labels = border_notation["right"]

        self.setup_patterns()
        for label, notation in pairs:
            idx = self.index_by_label[Label.fromJson(label)]
            self.fields[idx].notation = notation
        self.fields_setup()

    def show_text_message(self, text, delay=None):
        if self._text_message is not None:
            self._text_message.actual = False
        self._text_message = TextMessage(self, text, delay=delay)

    def hide_text_message(self):
        if self._text_message is not None:
            self._text_message.actual = False
        self._text_message = None
        self.invalidate()

    def json(self):
        board = []
        for idx in self.fields:
            piece = self.fields[idx].piece
            if piece:
                board.append([self.fields[idx].label.json(), piece.json()])
        return board
    
    def empty(self):
        self._board = {}
        for idx in self.fields:
            self.fields[idx].piece = None
        self.invalidate()
        self.repaint()

    def reset(self):
        self.fields_setup()

    def invalidate(self):
        for field in self.fields.values():
            field.invalidate()
        self._pixmap = None

    def piece_counts(self):
        if self._board is None:
            return 0, 0, 0, 0
        by_piece = defaultdict(int)
        for label in self.field_by_label:
            piece = self._board.get(label, None)
            if piece is not None:
                by_piece[piece] += 1

        first_men = by_piece.get(Piece(MAN, FIRST), 0)
        first_kings = by_piece.get(Piece(KING, FIRST), 0)
        second_men = by_piece.get(Piece(MAN, SECOND), 0)
        second_kings = by_piece.get(Piece(KING, SECOND), 0)

        return first_men, first_kings, second_men, second_kings

    @handling_error
    def fields_setup(self, board=None):
        if board is None:
            if self.game.game_id is None:
                board = dict()
            else:
                board = self.game.get_board()
                #print(board)

        self._board = board

        for label in self.field_by_label:
            field = self.field_by_label[label]
            field.invert_colors = self.invert_colors
            if label in board:
                field.piece = board[label]
            else:
                field.piece = None

        self.on_fields_setup.emit()
        self.invalidate()

    def show_board(self, board):
        self.fields_setup(board)
        self.repaint()

    def get_size_data(self):
        width = self.size().width()
        height = self.size().height()

        if self.show_border:
            row_height = height // (self.n_rows + 2*BORDER_WIDTH_COEF)
            col_width = width // (self.n_cols + 2*BORDER_WIDTH_COEF)
        else:
            row_height = height // self.n_rows
            col_width = width // self.n_cols
        cell_size = min(row_height, col_width)

        board_height = cell_size * self.n_rows
        board_width = cell_size * self.n_cols

        border_width = math.floor(BORDER_WIDTH_COEF * cell_size)
        if self.show_border:
            total_height = board_height + 2*border_width
            total_width = board_width + 2*border_width
        else:
            total_height = board_height
            total_width = board_width

        init_x = (width - total_width) / 2.0
        init_y = (height - total_height) / 2.0

        sz = SizeData(init_x, init_y, cell_size)
        sz.board_width = board_width
        sz.board_height = board_height
        sz.border_width = border_width
        if self.show_border:
            sz.init_x += border_width
            sz.init_y += border_width

        #print(f"{width}x{height} => {col_width}x{row_height} => {cell_size}, {board_width}x{board_height}, ({sz.init_x}; {sz.init_y}), ({sz.border_init_x}; {sz.border_init_y})")
        return sz

    def draw_field(self, painter, field, row, col, hide=False):
        sz = self.get_size_data()

        prev_hide_piece = field.hide_piece
        if hide:
            field.hide_piece = hide

        prev_possible_piece = field.possible_piece
        label = self.fields[(row,col)].label
        if self.show_possible_moves:
            if self._selected_field is not None and self._valid_target_fields is not None and label in self._valid_target_fields:
                field.possible_piece = self.fields[self._selected_field].piece
            if self._moveable_fields is not None:
                if not self.locked:
                    field.moveable = label in self._moveable_fields

        prev_captured = field.captured
        if self.move_animation.is_active() and label in self.move_animation.captured_fields:
            field.captured = True

        if self.flip:
            x = (self.n_cols-1-col) * sz.cell_size
            y = row * sz.cell_size
        else:
            x = col * sz.cell_size
            y = (self.n_rows-1-row) * sz.cell_size
        field.draw(painter, QRect(sz.init_x + x, sz.init_y + y, sz.cell_size, sz.cell_size))
        if hide:
            field.hide_piece = prev_hide_piece
        field.possible_piece = prev_possible_piece
        field.captured = prev_captured

        self.possible_captures_animation.draw_field(painter, field, sz, x, y)

    def draw(self):
        if self._pixmap is not None:
            return

        pixmap = QPixmap(self.size())
        pixmap.fill(Qt.white)
        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.Antialiasing, True)

        self.draw_background(painter)

        if self.show_border:
            self.draw_border(painter)

        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            hide = False
            is_start_field = ((row, col) == self.move_animation.start_field)
            is_end_field = ((row, col) == self.move_animation.end_field)
            if self.move_animation.is_active():
                hide = is_start_field or is_end_field
            elif self._drawing_my_move:
                hide = is_start_field
            self.draw_field(painter, field, row, col, hide=hide)

        if self.move_animation.is_active():
            piece = self.theme.get_piece(self.move_animation.piece)
            if piece is not None:
                w, h = piece.size().width(), piece.size().height()
                x0,y0 = self.move_animation.piece_position
                x = x0 - w*0.5
                y = y0 - h*0.5
                painter.drawPixmap(x, y, piece)

        if self.hint_moves is not None:
            for move in self.hint_moves:
                path = self._draw_hint_move_path(move)
                if self.theme.hint_border_color:
                    painter.strokePath(path, QPen(self.theme.hint_border_color))
                painter.setPen(QPen())
                painter.fillPath(path, QBrush(self.theme.hint_color))

        if self._text_message and self._text_message.show:
            self.draw_message(painter, self._text_message.message)

        painter.end()

        self._pixmap = pixmap
        self.update()

    def draw_border(self, painter):
        sz = self.get_size_data()

        def draw_label(rect, label):
            text_flags = Qt.AlignHCenter | Qt.AlignVCenter
            font = painter.font()
            font.setPointSize(sz.cell_size * 0.3)
            font.setBold(True)
            painter.setFont(font)
            painter.setPen(self.theme.border_notation_color)
            painter.drawText(rect, text_flags, label)

        # top
        cell_width = sz.cell_size
        cell_height = sz.border_width
        rect = QRectF(sz.init_x, sz.border_init_y, sz.board_width, sz.border_width)
        painter.drawTiledPixmap(rect, self.theme.border_top.get((cell_width, cell_height)))
        for col in range(self.n_cols):
            rect = QRect(sz.init_x + col*cell_width, sz.border_init_y, cell_width, cell_height)
            i = self.n_cols - col - 1 if self.flip else col
            label = self.top_border_labels[i]
            draw_label(rect, label)

        # bottom
        rect = QRectF(sz.init_x, sz.init_y + sz.board_height, sz.board_width, sz.border_width)
        painter.drawTiledPixmap(rect, self.theme.border_bottom.get((cell_width, cell_height)))
        for col in range(self.n_cols):
            rect = QRect(sz.init_x + col*cell_width, sz.init_y + sz.board_height, cell_width, cell_height)
            i = self.n_cols - col - 1 if self.flip else col
            label = self.bottom_border_labels[i]
            draw_label(rect, label)
        
        # left
        cell_width = sz.border_width
        cell_height = sz.cell_size
        rect = QRectF(sz.border_init_x, sz.init_y, sz.border_width, sz.board_height)
        painter.drawTiledPixmap(rect, self.theme.border_left.get((cell_width, cell_height)))
        for row in range(self.n_rows):
            rect = QRect(sz.border_init_x, sz.init_y + row*cell_height, cell_width, cell_height)
            i = row if self.flip else self.n_rows - row - 1
            label = self.left_border_labels[i]
            draw_label(rect, label)

        # right
        rect = QRectF(sz.init_x + sz.board_width, sz.init_y, sz.border_width, sz.board_height)
        painter.drawTiledPixmap(rect, self.theme.border_right.get((cell_width, cell_height)))
        for row in range(self.n_rows):
            rect = QRect(sz.init_x + sz.board_width, sz.init_y + row*cell_height, cell_width, cell_height)
            i = row if self.flip else self.n_rows - row - 1
            label = self.right_border_labels[i]
            draw_label(rect, label)

        # top-left
        cell_width = cell_height = sz.border_width
        rect = QRect(sz.border_init_x, sz.border_init_y, cell_width, cell_height)
        painter.drawPixmap(rect.topLeft(), self.theme.border_tl.get((cell_width, cell_height)))

        # top-right
        rect = QRect(sz.init_x + sz.board_width, sz.border_init_y, cell_width, cell_height)
        painter.drawPixmap(rect.topLeft(), self.theme.border_tr.get((cell_width, cell_height)))

        # bottom-left
        rect = QRect(sz.border_init_x, sz.init_y + sz.board_height, cell_width, cell_height)
        painter.drawPixmap(rect.topLeft(), self.theme.border_bl.get((cell_width, cell_height)))

        # bottom-right
        rect = QRect(sz.init_x + sz.board_width, sz.init_y + sz.board_height, cell_width, cell_height)
        painter.drawPixmap(rect.topLeft(), self.theme.border_br.get((cell_width, cell_height)))

        if self.theme.border_line_color:
            rect = QRect(sz.init_x, sz.init_y, sz.board_width, sz.board_height)
            painter.setPen(self.theme.border_line_color)
            painter.drawRect(rect)

    def draw_background(self, painter):
        if self.theme.background_color is not None:
            painter.fillRect(self.rect(), self.theme.background_color)
        else:
            painter.fillRect(self.rect(), self.palette().window())

        if self.theme.background_image.defined:
            if self.theme.background_style == 'SCALED':
                width = self.size().width()
                height = self.size().height()
                background = self.theme.background_image.get((width, height))
                painter.drawPixmap(self.rect(), background)
            elif self.theme.background_style == 'TILED':
                background = self.theme.background_image.get(None)
                painter.drawTiledPixmap(self.rect(), background)

    def draw_message(self, painter, text):
        flags = Qt.AlignHCenter | Qt.AlignVCenter | Qt.TextWordWrap
#         box = painter.boundingRect(self.rect(), flags, text)
#         height_r = self.height() / box.height()
#         width_r = self.width() / box.width()
#         r = min(height_r, width_r)
        font = painter.font()
#         font.setPointSize(font.pointSize() * r)
        font.setPointSize(self.theme.size / 2)
        font.setBold(True)
        painter.setFont(font)
        painter.setPen(self.theme.message_color)
        painter.drawText(self.get_board_rect(), flags, text)

    def _draw_hint_move_path(self, move):
        path = QPainterPath()
        start = QPointF(*self.get_field_center(self.index_by_label[move.from_field]))
        path.moveTo(start)
        prev = start
        for step in move.steps:
            pos = QPointF(*self.get_field_center(self.index_by_label[step.field]))
            path.lineTo(pos)
            last_line = pos - prev
            prev = pos
        sz = math.sqrt(last_line.x()**2 + last_line.y()**2)
        direction = last_line / sz
        head_size = self._get_hint_arrow_head_size()
        w = 0.2 * head_size * QPointF(-direction.y(), direction.x())
        p1 = pos + w
        p2 = pos + 0.3*head_size*direction
        p3 = pos - w
        path.moveTo(p1)
        path.lineTo(p2)
        path.lineTo(p3)

        pen = QPen(self.theme.hint_color, self._get_hint_arrow_width(), cap=Qt.RoundCap, join=Qt.MiterJoin)
        stroker = QPainterPathStroker(pen)
        path = stroker.createStroke(path).simplified()
        return path

    def get_size(self):
        field_size = self.get_target_field_size(self.size())
        return (field_size * self.n_cols, field_size * self.n_rows)
    
    def _get_hint_arrow_width(self):
        field_size = self.get_target_field_size(self.size())
        return 0.2*field_size

    def _get_hint_arrow_head_size(self):
        field_size = self.get_target_field_size(self.size())
        return field_size

    def get_board_rect(self):
        w, h = self.get_size()
        sz = self.get_size_data()
        return QRect(sz.init_x, sz.init_y, w, h)

    def get_target_field_size(self, size):
        w_max = size.width()
        h_max = size.height()

        if self.show_border:
            r_w = w_max // (self.n_cols + 2*BORDER_WIDTH_COEF)
            r_h = h_max // (self.n_rows + 2*BORDER_WIDTH_COEF)
        else:
            r_w = w_max // self.n_cols
            r_h = h_max // self.n_rows

        return min(r_w, r_h)
    
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
            #self.fields_setup()
            self.invalidate()

    def get_field_center(self, idx):
        sz = self.get_size_data()
        row, col = idx
        (width, height) = self.get_size()
        if self.flip:
            x = (self.n_cols - 1 - col + 0.5) / self.n_cols
            y = (row + 0.5) / self.n_rows
        else:
            x = (col + 0.5) / self.n_cols
            y = (self.n_rows - 1 - row + 0.5) / self.n_rows
        #logging.debug("{} => {}".format(idx, (x,y)))
        return (sz.init_x + x*width, sz.init_y + y*height)

    def get_move_end_field(self, move):
        label = move.steps[-1].field
        return self.index_by_label[label]
    
    def timerEvent(self, e):
        if e.timerId() == self._text_message_timer:
            pass
        else:
            if self.move_animation.tick(e) or self.possible_captures_animation.tick(e):
                self._pixmap = None
                self.repaint()
    
    def _is_mine(self, piece):
        return piece is not None and piece.side == self.game.user_side

    def _show_forbidden_cursor(self):
        self.setCursor(Qt.ForbiddenCursor)
        QTimer.singleShot(300, lambda: self.setCursor(Qt.ArrowCursor))

    def _highlight_captures(self):
        return self.settings.value("highlight_captures", SHOW_ON_CLICK)

    @handling_error
    def process_click(self, row, col):
        self.field_clicked.emit(row, col)
        if self.toplevel.board_setup_mode:
            return

        field = self.fields[(row, col)]
        if field.notation is None:
            return
        piece = self._board.get(field.label)
        if self._is_mine(piece):
            moves = self.game.get_possible_moves(field.label)
            if not moves:
                if self._highlight_captures() == SHOW_ON_CLICK:
                    all_moves = self.game.get_possible_moves()
                    self.possible_captures_animation.start(all_moves)
                logging.warning(_("Piece at {} does not have moves").format(field.notation))
                self._show_forbidden_cursor()
            else:
                self._valid_target_fields = dict((move.steps[-1].field, move) for move in moves)
                valid_targets = []
                for label in self._valid_target_fields:
                    valid_targets.append(self.fields[(label.row, label.col)].notation)
                logging.info(_("Possible target fields: {}").format(", ".join(valid_targets)))
                self.selected_field = (row, col)
                self.repaint()
        elif self.selected_field is not None and self._valid_target_fields is not None:
            if field.label in self._valid_target_fields:
                src_index = self.selected_field
                self.selected_field = None
                self.fields[src_index].moveable = False
                src_field = self.fields[src_index].label
                dst_field = field.label
                self.game.begin_move(src_field, dst_field)
                self.my_turn = False
                self.message.emit(WaitingMove())
                self.last_left = self.index_by_label[field.label]
                self.last_moved = (row,col)

                self.hint_moves = None
                move = self._valid_target_fields[field.label]
                start_position = self.get_field_center(self.index_by_label[src_field])
                piece = self.fields[src_index].piece
                if piece is None:
                    raise Exception("No piece at {}!".format(src_index))
                if self.invert_colors:
                    piece = piece.inverted()
                self._drawing_my_move = True
                self.move_animation.start(src_index, (row, col), move, start_position, piece, process_result=True)
                self._valid_target_fields = None
                self.repaint()
            else:
                logging.warning(_("Piece at {} cannot be moved to {}").format(self.fields[self.selected_field].notation, field.notation))
                self._show_forbidden_cursor()

        else:
            logging.warning(_("Field {} does not belong to you").format(field.notation))
            self._show_forbidden_cursor()

    def get_notation(self, label):
        idx = self.index_by_label[label]
        return self.fields[idx].notation
    
    def show_move(self, move):
        first = self.get_notation(move.from_field)
        last = self.get_notation(move.steps[-1].field)
        is_capture = any(step.capture == True for step in move.steps)
        if is_capture:
            return "{}x{}".format(first, last)
        else:
            return "{}-{}".format(first, last)
    
    def start_move_animation(self, move):
        src_field = self.index_by_label[move.from_field]
        dst_field = self.get_move_end_field(move)
        start_position = self.get_field_center(src_field)
        piece = self.fields[src_field].piece
        if piece is None:
            raise Exception("No piece at {}!".format(src_field))
        if self.invert_colors:
            piece = piece.inverted()
        self.move_animation.start(src_field, dst_field, move, start_position, piece, process_result = False)

    def process_message(self, message):
        if "move" in message:
            move = Move.fromJson(message["move"])
            self.message.emit(OtherSideMove(self, move))
            self._new_board = Game.parse_board(message["board"])
            self.start_move_animation(move)
            my_side = 'First' if self.game.user_side == FIRST else 'Second'
            self.my_turn = True
            #self.my_turn = message["to_side"] == my_side
        elif "undo" in message:
            self.message.emit(UndoMessage())
            self._board = Game.parse_board(message["board"])
        elif "hint" in message:
            moves = [Move.fromJson(m) for m in message["hint"]]
            self.message.emit(AiHintMessage(moves))
        elif "result" in message:
            result = message["result"]
            self.message.emit(GameResultMessage(result))
        elif "draw" in message:
            self.message.emit(DrawRequestedMessage())
        elif "draw_accepted" in message:
            result = message["draw_accepted"]
            self.message.emit(DrawResponseMessage(result))
        elif "message" in message:
            text = message["message"]
            level = message["level"]
            self.server_log.emit(level, text)

    @handling_error
    def on_animation_finished(self, process_result):
        if process_result:
            res = self.game.get_move_result()
            if res: 
                board, session, messages = res
                self._board = board
                self._drawing_my_move = True
                for message in messages:
                    self.process_message(message)
                self.selected_field = None
                self.toplevel.ai_session = session
        elif self._new_board is not None:
            self._board = self._new_board
            self._new_board = None
        self.last_left = self.move_animation.start_field
        self.last_moved = self.move_animation.end_field
        self.fields_setup(self._board)
        self.repaint()
        self._drawing_my_move = False

    def _is_mouse_active(self):
        if self.move_animation.is_active():
            return False
        
        if not self.my_turn:
            return False
        if self.locked:
            return False

        if self.game.draw_state == WE_REQUESTED_DRAW:
            #logging.warning(_("Awaiting a response about draw."))
            return False
        elif self.game.draw_state == DRAW_REQUESTED_FROM_US:
            logging.warning(_("Another side have offered a draw. You have to accept or decline it."))
            return False
        
        return True

    def _get_field_at_pos(self, pos):
        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            if field is not None and field.rect().contains(pos):
                return (row, col)
        return None

    @handling_error
    def mousePressEvent(self, me):
        if me.button() != Qt.LeftButton:
            return
        
        if not self._is_mouse_active():
            return
        
        at_pos = self._get_field_at_pos(me.pos())
        if at_pos is not None:
            (row, col) = at_pos
            self.invalidate()
            self.update()
            self.process_click(row, col)

    @handling_error
    def mouseMoveEvent(self, me):
        if not self._is_mouse_active():
            return

        cursor = Qt.ArrowCursor
        at_pos = self._get_field_at_pos(me.pos())
        if at_pos is not None and self._prev_hovered_field != at_pos:
            (row, col) = at_pos
            field = self.fields[(row, col)]
            if field.notation is not None:
                piece = self._board.get(field.label)
                if self._is_mine(piece):
                    moves = self.game.get_possible_moves(field.label)
                    if moves:
                        cursor = Qt.OpenHandCursor
                elif piece is None and self.selected_field is not None and self._valid_target_fields is not None:
                    if field.label in self._valid_target_fields:
                        cursor = Qt.PointingHandCursor
            self._prev_hovered_field = at_pos
            self.setCursor(cursor)

    def _handle_connection_error(self, url, e):
        self.toplevel._handle_connection_error(url, e)

    def _handle_game_error(self, rs):
        self.toplevel._handle_game_error(rs)

    def save_image(self, path):
        self.draw()

        img = QImage(self.width(), self.height(), QImage.Format_RGB32)
        painter = QPainter(img)
        painter.drawPixmap(0, 0, self._pixmap)
        painter.end()
        img.save(path)

