
from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, Qt
from PyQt5.QtWidgets import QApplication, QWidget

from hcheckers import common

class Field(object):
    def __init__(self):
        self._show_frame = False
        self._moveable = False
        self._last_moved = False
        self._last_left = False
        self._show_label = False
        self._pattern_id = None
        self._piece = None
        self._possible_piece = None
        self._label = None
        self._notation = None
        self._notation_above = False
        self._pixmap = None
        self._rect = None
        self._theme = None
        self._hide_piece = False
        self._captured = False
        self.invert_colors = False
        self.usable = False

    def get_captured(self):
        return self._captured

    def set_captured(self, value):
        self._captured = value
        self.invalidate()

    captured = property(get_captured, set_captured)

    def get_label(self):
        return self._label

    def set_label(self, label):
        self._label = label
        self.invalidate()

    label = property(get_label, set_label)

    def get_notation(self):
        return self._notation

    def set_notation(self, value):
        self._notation = value
        self.invalidate()

    notation = property(get_notation, set_notation)

    def get_show_frame(self):
        return self._show_frame

    def set_show_frame(self, value):
        self._show_frame = value
        self.invalidate()

    show_frame = property(get_show_frame, set_show_frame)

    def get_moveable(self):
        return self._moveable

    def set_moveable(self, value):
        self._moveable = value
        self.invalidate()

    moveable = property(get_moveable, set_moveable)

    def get_last_moved(self):
        return self._last_moved

    def set_last_moved(self, value):
        self._last_moved = value
        self.invalidate()

    last_moved = property(get_last_moved, set_last_moved)

    def get_last_left(self):
        return self._last_left

    def set_last_left(self, value):
        self._last_left = value
        self.invalidate()

    last_left = property(get_last_left, set_last_left)

    def get_theme(self):
        return self._theme

    def set_theme(self, theme):
        self._theme = theme
        self.invalidate()

    theme = property(get_theme, set_theme)

    def get_notation_above(self):
        return self._notation_above

    def set_notation_above(self, value):
        self._notation_above = value
        self.invalidate()

    notation_above = property(get_notation_above, set_notation_above)

    def get_pattern_id(self):
        return self._pattern_id

    def set_pattern_id(self, i):
        self._pattern_id = i
        self.invalidate()

    pattern_id = property(get_pattern_id, set_pattern_id)

    def get_piece(self):
        return self._piece

    def set_piece(self, piece):
        self._piece = piece
        self.invalidate()

    piece = property(get_piece, set_piece)

    def get_possible_piece(self):
        return self._possible_piece

    def set_possible_piece(self, piece):
        self._possible_piece = piece
        self.invalidate()

    possible_piece = property(get_possible_piece, set_possible_piece)

    def get_hide_piece(self):
        return self._hide_piece
    
    def set_hide_piece(self, hide):
        self._hide_piece = hide
        self.invalidate()

    hide_piece = property(get_hide_piece, set_hide_piece)

    def get_show_label(self):
        return self._show_label

    def set_show_label(self, value):
        self._show_label = value
        self.invalidate()

    show_label = property(get_show_label, set_show_label)

    def invalidate(self):
        self._pixmap = None

    def rect(self):
        return self._rect

    def _draw_piece(self, painter):
        piece = self._theme.get_piece(self.piece, invert=self.invert_colors)
        possible_piece = self._theme.get_piece(self.possible_piece, invert=self.invert_colors)
        if piece is not None and not self.hide_piece:
            painter.drawPixmap(0, 0, piece)
        elif possible_piece is not None:
            painter.setOpacity(0.5)
            painter.drawPixmap(0, 0, possible_piece)
            painter.setOpacity(1.0)

    def _draw(self):
        if self._pixmap is not None:
            return
        
        self._pixmap = QPixmap(self._rect.width(), self._rect.height())
        self._pixmap.fill(Qt.white)
        painter = QPainter()
        painter.begin(self._pixmap)

        if self._pattern_id:
            pattern = self._theme.get_pattern(self._pattern_id)
            painter.drawPixmap(0, 0, pattern)

        if self.show_frame:
            frame = self._theme.get_frame()
            painter.drawPixmap(0, 0, frame)
        elif self.last_moved:
            frame = self._theme.get_last_moved()
            if frame:
                painter.drawPixmap(0, 0, frame)
        elif self.last_left:
            frame = self._theme.get_last_left()
            if frame:
                painter.drawPixmap(0, 0, frame)
        elif self.moveable:
            frame = self._theme.get_moveable()
            if frame:
                painter.drawPixmap(0, 0, frame)

        # notation
        painter.setPen(self._theme.field_notation_color)
        notation_rect = painter.boundingRect(2, 2, 0, 0, Qt.AlignLeft, self.notation)
        if self.notation_above:
            self._draw_piece(painter)
            if self.show_label:
                if self._theme.field_notation_background:
                    painter.fillRect(notation_rect, self._theme.field_notation_background)
                painter.drawText(notation_rect, Qt.AlignTop | Qt.AlignLeft, self.notation)
        else:
            if self.show_label:
                if self._theme.field_notation_background:
                    painter.fillRect(notation_rect, self._theme.field_notation_background)
                painter.drawText(notation_rect, Qt.AlignTop | Qt.AlignLeft, self.notation)
            self._draw_piece(painter)

        if self.captured:
            captured = self._theme.get_captured()
            painter.drawPixmap(0, 0, captured)

        painter.end()

    def draw(self, painter, rect):
        self._rect = rect
        self._draw()
        painter.drawPixmap(rect.x(), rect.y(), self._pixmap)

