
import math
import logging

from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal
from PyQt5.QtWidgets import QApplication, QWidget

from hcheckers.common import *
from hcheckers.field import Field
from hcheckers.game import Game, RequestError

ANIMATION_STEP_DURATION = 50
ANIMATION_STEPS_PER_STEP = 10

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

    finished = pyqtSignal(bool)

    def start(self, start_field, end_field, move, start_position, piece, process_result = True):
        if piece is None:
            raise Exception("piece is None")
        self.piece = piece
        self.piece_position = start_position
        self.step = 0
        self.steps = ANIMATION_STEPS_PER_STEP * len(move.steps)
        self.move = move
        self.captured_fields = [step.field for step in move.steps if step.capture == True]
        self.start_field = start_field
        self.end_field = end_field
        self.process_result = process_result
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
    def __init__(self, theme, settings, game, toplevel):
        QWidget.__init__(self, toplevel)

        self.toplevel = toplevel
        self.game = game
        self._theme = theme
        self.settings = settings
        self._show_notation = settings.value("show_notation", type=bool)
        self.show_possible_moves = settings.value("show_possible_moves", type=bool)
        self.invert_colors = False
        self._flip = False

        self._init_fields(8, 8)

        self._pixmap = None

        self._selected_field = None
        self._valid_target_fields = None
        self._moveable_fields = None
        self._last_moved_field = None
        self._board = dict()
        self._new_board = None
        self._text_message = None

        self._my_turn = True
        self.locked = False

        self.move_animation = MoveAnimation(self)
        self.move_animation.finished.connect(self.on_animation_finished)

        self.setup_patterns()

    field_clicked = pyqtSignal(int, int)

    message = pyqtSignal(object)

    server_log = pyqtSignal(str, str)

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
        else:
            self.setCursor(Qt.WaitCursor)
            self._moveable_fields = None

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

    def get_theme(self):
        return self._theme

    def setup_patterns(self):
        for row in range(self.n_rows):
            for col in range(self.n_cols):
                if (row % 2) == (col % 2):
                    self.fields[(row,col)].pattern_id = 2
                    self.fields[(row,col)].usable = True
                else:
                    self.fields[(row,col)].pattern_id = 1
                    self.fields[(row,col)].usable = False

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

    def get_show_notation(self):
        return self._show_notation
    
    def set_show_notation(self, value):
        self._show_notation = value
        for idx in self.fields:
            self.fields[idx].show_label = value
        self.invalidate()
        self.repaint()

    show_notation = property(get_show_notation, set_show_notation)

    def get_flip(self):
        return self._flip

    def set_flip(self, value):
        self._flip = value
        self.invalidate()
        self.repaint()

    flip = property(get_flip, set_flip)

    def set_notation(self, size, pairs):
        (rows, cols) = size
        self._init_fields(rows, cols)
        self.setup_patterns()
        for label, notation in pairs:
            idx = self.index_by_label[Label.fromJson(label)]
            self.fields[idx].notation = notation
        self.fields_setup()

    def get_text_message(self):
        return self._text_message

    def set_text_message(self, text):
        self._text_message = text
        self.invalidate()
        #self.repaint()

    text_message = property(get_text_message, set_text_message)

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

    @handling_error
    def fields_setup(self, board=None):
        if board is None:
            if self.game.game_id is None:
                board = dict()
            else:
                board = self.game.get_board()

        self._board = board

        for label in self.field_by_label:
            field = self.field_by_label[label]
            field.invert_colors = self.invert_colors
            if label in board:
                field.piece = board[label]
            else:
                field.piece = None

        self.invalidate()

    def draw_field(self, painter, field, row, col, hide=False):
        width = self.size().width()
        height = self.size().height()

        row_height = height // self.n_rows
        col_width = width // self.n_cols
        size = min(row_height, col_width)

        prev_hide_piece = field.hide_piece
        if hide:
            field.hide_piece = hide

        prev_possible_piece = field.possible_piece
        label = self.fields[(row,col)].label
        if self.show_possible_moves:
            if self._selected_field is not None and self._valid_target_fields is not None and label in self._valid_target_fields:
                field.possible_piece = self.fields[self._selected_field].piece
            if self._moveable_fields is not None:
                field.moveable = label in self._moveable_fields

        prev_captured = field.captured
        if self.move_animation.is_active() and label in self.move_animation.captured_fields:
            field.captured = True

        if self.flip:
            x = (self.n_cols-1-col) * size
            y = row * size
        else:
            x = col * size
            y = (self.n_rows-1-row) * size
        field.draw(painter, QRect(x, y, size, size))
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
        painter.setRenderHint(QPainter.Antialiasing, True)

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

        if self.text_message:
            self.drawText(painter, self.text_message)

        painter.end()

        self._pixmap = pixmap
        self.update()

    def drawText(self, painter, text):
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
        painter.drawText(self.rect(), flags, text)

    def get_size(self):
        field_size = self.get_target_field_size(self.size())
        return (field_size * self.n_cols, field_size * self.n_rows)

    def get_target_field_size(self, size):
        w_max = size.width()
        h_max = size.height()
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
            self.fields_setup()

    def get_field_center(self, idx):
        row, col = idx
        (width, height) = self.get_size()
        if self.flip:
            x = (self.n_cols - 1 - col + 0.5) / self.n_cols
            y = (row + 0.5) / self.n_rows
        else:
            x = (col + 0.5) / self.n_cols
            y = (self.n_rows - 1 - row + 0.5) / self.n_rows
        #logging.debug("{} => {}".format(idx, (x,y)))
        return (x*width, y*height)

    def get_move_end_field(self, move):
        label = move.steps[-1].field
        return self.index_by_label[label]
    
    def timerEvent(self, e):
        if self.move_animation.tick(e):
            self._pixmap = None
            self.repaint()

    @handling_error
    def process_click(self, row, col):
        self.field_clicked.emit(row, col)
        if self.toplevel.board_setup_mode:
            return

        field = self.fields[(row, col)]
        if field.notation is None:
            return
        piece = self._board.get(field.label)
        if piece is not None and piece.side == self.game.user_side:
            moves = self.game.get_possible_moves(field.label)
            #logging.debug(moves)
            if not moves:
                logging.warning(_("Piece at {} does not have moves").format(field.notation))
            else:
                self._valid_target_fields = dict((move.steps[-1].field, move) for move in moves)
                valid_targets = []
                for label in self._valid_target_fields:
                    valid_targets.append(self.fields[(label.row, label.col)].notation)
                logging.info(_("Possible target fields: {}").format(", ".join(valid_targets)))
                self.selected_field = (row, col)
                self.repaint()
        elif piece is None and self.selected_field is not None and self._valid_target_fields is not None:
            if field.label in self._valid_target_fields:
                src_index = self.selected_field
                self.selected_field = None
                self.fields[src_index].moveable = False
                src_field = self.fields[src_index].label
                dst_field = field.label
                self.game.begin_move(src_field, dst_field)
                self.my_turn = False
                self.message.emit(WaitingMove())
                self.last_moved = (row,col)

                move = self._valid_target_fields[field.label]
                start_position = self.get_field_center(self.index_by_label[src_field])
                piece = self.fields[src_index].piece
                if self.invert_colors:
                    piece = piece.inverted()
                self.move_animation.start(src_index, (row, col), move, start_position, piece, process_result=True)
                self._valid_target_fields = None
                self.repaint()
            else:
                logging.warning(_("Piece at {} cannot be moved to {}").format(self.fields[self.selected_field].notation, field.notation))

        else:
            logging.warning(_("Field {} does not belong to you").format(field.notation))

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

    def process_message(self, message):
        if "move" in message:
            move = Move.fromJson(message["move"])
            self.message.emit(OtherSideMove(self, move))
            self._new_board = Game.parse_board(message["board"])
            src_field = self.index_by_label[move.from_field]
            dst_field = self.get_move_end_field(move)
            start_position = self.get_field_center(src_field)
            piece = self.fields[src_field].piece
            if self.invert_colors:
                piece = piece.inverted()
            self.move_animation.start(src_field, dst_field, move, start_position, piece, process_result = False)
            my_side = 'First' if self.game.user_side == FIRST else 'Second'
            self.my_turn = True
            #self.my_turn = message["to_side"] == my_side
        elif "undo" in message:
            self.message.emit(UndoMessage())
            self._board = Game.parse_board(message["board"])
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
                board, messages = res
                self._board = board
                for message in messages:
                    self.process_message(message)
                self.selected_field = None
        elif self._new_board is not None:
            self._board = self._new_board
            self._new_board = None
        self.last_moved = self.move_animation.end_field
        if not self.game.finished:
            self.text_message = None
        self.fields_setup(self._board)
        self.repaint()

    @handling_error
    def mousePressEvent(self, me):
        if me.button() != Qt.LeftButton:
            return
        
        if self.move_animation.is_active():
            return
        
        if not self.my_turn:
            return
        if self.locked:
            return

        if self.game.draw_state == WE_REQUESTED_DRAW:
            logging.warning(_("Awaiting a response about draw."))
            return
        elif self.game.draw_state == DRAW_REQUESTED_FROM_US:
            logging.warning(_("Another side have offered a draw. You have to accept or decline it."))
            return
        
        for (row, col) in self.fields:
            field = self.fields[(row, col)]
            if field.rect().contains(me.pos()):
                self.invalidate()
                self.update()
                self.process_click(row, col)
                return

    def _handle_connection_error(self, url, e):
        self.toplevel._handle_connection_error(url, e)

    def _handle_game_error(self, rs):
        self.toplevel._handle_game_error(rs)

