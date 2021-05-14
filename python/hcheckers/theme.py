
import os
from os.path import join, basename, isdir, exists, abspath
import logging

from PyQt5.QtCore import Qt, QSettings, QTimer
from PyQt5.QtGui import QPixmap, QPainter, QColor
from PyQt5.Qt import QSvgRenderer
from PyQt5.QtMultimedia import QSound

from hcheckers.common import *

class CachedPixmap(object):
    def __init__(self, path, color=None):
        self.path = path
        self.size = None
        self._pixmap = None
        self.is_image = exists(path)
        self.color = color
        self.defined = self.is_image or (color is not None)
        if not self.defined:
            logging.debug(f"{self.path} wasn't found")

    def invalidate(self):
        self.size = None
        self._pixmap = None

    def _get(self, size):
        self.size = size
        if self.is_image:
            if self.path.endswith(".svg"):
                renderer = QSvgRenderer(self.path)
                if size is None:
                    self.size = size = renderer.defaultSize().width(), renderer.defaultSize().height()
                #print("Rendering SVG {}: {}".format(self.path, size))
                pixmap = QPixmap(*size)
                pixmap.fill(Qt.transparent)
                painter = QPainter(pixmap)
                renderer.render(painter)
                painter.end()
            else:
                if size is None:
                    pixmap = QPixmap(self.path)
                    self.size = pixmap.width(), pixmap.height()
                else:
                    pixmap = QPixmap(self.path).scaled(*size)
        else:
            if size is None:
                pixmap = QPixmap(256, 256)
                self.size = pixmap.width(), pixmap.height()
            else:
                pixmap = QPixmap(*size)
            pixmap.fill(self.color)
        return pixmap

    def get(self, size):
        if not self.defined:
            return None
        if isinstance(size, (int, float)):
            size = (size, size)

        if self.size == size and self._pixmap is not None:
            #logging.debug("{}: already rendered".format(self.path))
            return self._pixmap
        else:
            self._pixmap = self._get(size)
            return self._pixmap

class Sound(object):
    def __init__(self, sound):
        self.sound = sound
        self.loop = 0
        self.loops = 0
        self.duration = 0

        self.timer = QTimer()
        self.timer.setSingleShot(False)
        self.timer.timeout.connect(self._on_timer)

    def is_defined(self):
        return self.sound is not None

    def play(self, loops, duration):
        if self.sound is not None:
            self.loop = 0
            self.loops = loops
            self.duration = duration
            self.timer.start(self.duration)
            self._play()

    def _play(self):
        self.sound.play()

    def _on_timer(self):
        self.sound.stop()
        self.loop = self.loop + 1
        if self.loop < self.loops:
            self._play()
        else:
            self.timer.stop()

    @classmethod
    def get(cls, path):
        if path is not None and exists(path):
            return Sound(QSound(path))
        else:
            return Sound(None)

class BorderField(CachedPixmap):
    def __init__(self, path, color=None):
        CachedPixmap.__init__(self, path, color)
        self.text = None
        self.text_size = None
        self.text_color = None

    @staticmethod
    def parse(base_path, s):
        path = join(base_path, s)
        if exists(path):
            field = BorderField(path=path)
        else:
            field = BorderField(path=None, color=QColor(s))
        return field

    def _get(self, size):
        pixmap = super()._get(size)

        painter = QPainter(pixmap)
        flags = Qt.AlignHCenter | Qt.AlignVCenter | Qt.TextWordWrap
        font = painter.font()
        font.setPointSize(self.text_size / 2)
        painter.setFont(font)
        painter.setPen(self.text_color)
        painter.drawText(self.rect(), flags, self.text)
        painter.end()

        return pixmap

class Theme(object):
    def __init__(self, path, size):
        self.path = path
        self.size = size
        settings = QSettings(join(path, "theme"), QSettings.IniFormat)

        self.id = basename(path) 
        self.name = settings.value("name", self.id)

        self.background_image = CachedPixmap(join(path, settings.value("background", "background.png")))
        self.background_color = None
        if not self.background_image.defined:
            color = settings.value("background")
            if color is not None:
                self.background_color = QColor(color)

        background_style = settings.value("background_style", "TILED")
        self.background_style = background_style.upper()
        if not self.background_style in {'TILED', 'SCALED'}:
            raise Exception("Unsupported background style: " + self.background_style)

        self.pattern1 = CachedPixmap(join(path, settings.value("tile1", "tile1.svg")))
        self.pattern2 = CachedPixmap(join(path, settings.value("tile2", "tile2.svg")))
        self.frame = CachedPixmap(join(path, settings.value("frame", "frame.svg")))
        self.moveable = CachedPixmap(join(path, settings.value("moveable", "moveable.svg")))
        self.last_moved = CachedPixmap(join(path, settings.value("lastmoved", "lastmoved.svg")))
        self.captured = CachedPixmap(join(path, settings.value("captured", "captured.svg")))

        self.man_black = CachedPixmap(join(path, settings.value("man_black", "manblack.svg")))
        self.man_white = CachedPixmap(join(path, settings.value("man_white", "manwhite.svg")))
        self.king_black = CachedPixmap(join(path, settings.value("king_black", "kingblack.svg")))
        self.king_white = CachedPixmap(join(path, settings.value("king_white", "kingwhite.svg")))

        self.move_sound = Sound.get(abspath(join(path, settings.value("move_sound", "move.wav"))))
        self.check_sound = Sound.get(abspath(join(path, settings.value("check_sound", "check.wav"))))
        self.enable_sound = (self.move_sound.is_defined() or self.check_sound.is_defined())

        field_notation_background = settings.value("field_notation_background")
        if field_notation_background:
            self.field_notation_background = QColor(field_notation_background)
        else:
            self.field_notation_background = None

        field_notation_color = settings.value("field_notation_color", "white")
        self.field_notation_color = QColor(field_notation_color)

        border_notation_color = settings.value("border_notation_color", "white")
        self.border_notation_color = QColor(border_notation_color)

        self.border = BorderField.parse(path, settings.value("border", "gray"))

        message_color = settings.value("message_color", "black")
        self.message_color = QColor(message_color)

        hint_color = settings.value("hint_color", "#8800ff00")
        self.hint_color = QColor(hint_color)

    def reset(self):
        self.background_image.invalidate()
        self.pattern1.invalidate()
        self.pattern2.invalidate()
        self.frame.invalidate()
        self.moveable.invalidate()
        self.last_moved.invalidate()
        self.captured.invalidate()
        self.man_black.invalidate()
        self.man_white.invalidate()
        self.king_black.invalidate()
        self.king_white.invalidate()

    def set_target_size(self, size):
        if size != self.size:
            self.size = size
            self.reset()

    def get_frame(self):
        return self.frame.get(self.size)

    def get_moveable(self):
        return self.moveable.get(self.size)
    
    def get_last_moved(self):
        return self.last_moved.get(self.size)
    
    def get_captured(self):
        return self.captured.get(self.size)
    
    def get_pattern1(self):
        return self.pattern1.get(self.size)

    def get_pattern2(self):
        return self.pattern2.get(self.size)
    
    def get_pattern(self, i):
        if i == 1:
            return self.get_pattern1()
        else:
            return self.get_pattern2()

    def get_piece(self, piece, invert=False, size=None):
        if piece is None:
            return None
        if size is None:
            size = self.size
        if invert:
            piece = piece.inverted()
        if piece.kind == MAN and piece.side == FIRST:
            return self.man_white.get(size)
        elif piece.kind == MAN and piece.side == SECOND:
            return self.man_black.get(size)
        elif piece.kind == KING and piece.side == FIRST:
            return self.king_white.get(size)
        else:
            return self.king_black.get(size)

    def get_field_size(self):
        return self.man_white.get(None).width()

    def play_move(self, steps, duration):
        if self.enable_sound:
            self.move_sound.play(steps, duration)

    def play_check(self, steps, duration):
        if self.enable_sound:
            self.check_sound.play(steps, duration)

    @classmethod
    def list_themes(cls, share_dir):
        themes_dir = join(share_dir, "themes")
        result = []
        for name in os.listdir(themes_dir):
            path = join(themes_dir, name)
            if isdir(path):
                result.append(Theme(path, None))
        return result


