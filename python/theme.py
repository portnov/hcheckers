
from os.path import join, basename
from PyQt5.QtCore import Qt, QSettings
from PyQt5.QtGui import QPixmap, QPainter
from PyQt5.Qt import QSvgRenderer

from common import *

class CachedPixmap(object):
    def __init__(self, path):
        self.path = path
        self.size = None
        self._pixmap = None

    def invalidate(self):
        self.size = None
        self._pixmap = None

    def get(self, size):
        if self.size == size and self._pixmap is not None:
            #print("{}: already rendered".format(self.path))
            return self._pixmap
        else:
            self.size = size
            if self.path.endswith(".svg"):
                renderer = QSvgRenderer(self.path)
                if size is None:
                    self.size = size = renderer.defaultSize().width()
                print("Rendering SVG {}: {}".format(self.path, size))
                self._pixmap = QPixmap(size, size)
                self._pixmap.fill(Qt.transparent)
                painter = QPainter(self._pixmap)
                renderer.render(painter)
                painter.end()
            else:
                if size is None:
                    self._pixmap = QPixmap(self.path)
                    self.size = self._pixmap.width()
                else:
                    self._pixmap = QPixmap(self.path).scaled(size, size)
            return self._pixmap


class Theme(object):
    def __init__(self, path, size):
        self.path = path
        self.size = size
        settings = QSettings(join(path, "theme"), QSettings.IniFormat)

        self.name = settings.value("name", basename(path))

        self.pattern1 = CachedPixmap(join(path, settings.value("tile1", "tile1.svg")))
        self.pattern2 = CachedPixmap(join(path, settings.value("tile2", "tile2.svg")))
        self.frame = CachedPixmap(join(path, settings.value("frame", "frame.svg")))

        self.man_black = CachedPixmap(join(path, settings.value("man_black", "manblack.svg")))
        self.man_white = CachedPixmap(join(path, settings.value("man_white", "manwhite.svg")))
        self.king_black = CachedPixmap(join(path, settings.value("king_black", "kingblack.svg")))
        self.king_white = CachedPixmap(join(path, settings.value("king_white", "kingwhite.svg")))

    def reset(self):
        self.pattern1.invalidate()
        self.pattern2.invalidate()
        self.frame.invalidate()
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
    
    def get_pattern1(self):
        return self.pattern1.get(self.size)

    def get_pattern2(self):
        return self.pattern2.get(self.size)
    
    def get_pattern(self, i):
        if i == 1:
            return self.get_pattern1()
        else:
            return self.get_pattern2()

    def get_piece(self, piece):
        if piece is None:
            return None
        if piece.kind == MAN and piece.side == FIRST:
            return self.man_white.get(self.size)
        elif piece.kind == MAN and piece.side == SECOND:
            return self.man_black.get(self.size)
        elif piece.kind == KING and piece.side == FIRST:
            return self.king_white.get(self.size)
        else:
            return self.king_black.get(self.size)

    def get_field_size(self):
        return self.man_white.get(None).width()


