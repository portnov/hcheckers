
from PyQt5.QtGui import QPainter, QPixmap
from PyQt5.QtCore import QRect, Qt
from PyQt5.QtWidgets import QApplication, QWidget

import common

class Field(object):
    def __init__(self):
        self._show_frame = False
        self._show_label = False
        self._pattern_id = None
        self._piece = None
        self._label = None
        self._notation_above = False
        self._pixmap = None
        self._rect = None
        self._theme = None

    def get_label(self):
        return self._label

    def set_label(self, label):
        self._label = label
        self.invalidate()

    label = property(get_label, set_label)

    def get_show_frame(self):
        return self._show_frame

    def set_show_frame(self, value):
        self._show_frame = value
        self.invalidate()

    show_frame = property(get_show_frame, set_show_frame)

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

        # notation
        painter.setPen(Qt.white)
        notation_rect = painter.boundingRect(2, 2, 0, 0, Qt.AlignLeft, self.label)
        piece = self._theme.get_piece(self.piece)
        if self.notation_above:
            if piece is not None:
                painter.drawPixmap(0, 0, piece)
            if self.show_label:
                painter.fillRect(notation_rect, Qt.black)
                painter.drawText(notation_rect, Qt.AlignTop | Qt.AlignLeft, self.label)
        else:
            if self.show_label:
                painter.drawText(notation_rect, Qt.AlignTop | Qt.AlignLeft, self.label)
            if piece is not None:
                painter.drawPixmap(0, 0, piece)

        if self.show_frame:
            frame = self._theme.get_frame()
            painter.drawPixmap(0, 0, frame)

        painter.end()

    def draw(self, painter, rect):
        self._rect = rect
        self._draw()
        painter.drawPixmap(rect.x(), rect.y(), self._pixmap)

