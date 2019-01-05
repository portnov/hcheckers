
import sys
from os.path import join, exists, dirname
import os
import re
import logging

from PyQt5.QtGui import QIcon
from PyQt5.QtWidgets import QListWidget, QListWidgetItem

LOG_MASK = "Log files (*.log)"

class UiLogHandler(logging.Handler):
    def __init__(self, list_widget):
        logging.Handler.__init__(self)
        self.list_widget = list_widget

    def get_icon(self, record):
        if record.levelno == logging.INFO:
            return QIcon.fromTheme("dialog-information")
        elif record.levelno == logging.ERROR:
            return QIcon.fromTheme("dialog-error")
        elif record.levelno == logging.WARNING:
            return QIcon.fromTheme("dialog-warning")
        return None

    def emit(self, record):
        try:
            msg = self.format(record)
            item = QListWidgetItem(self.list_widget)
            item.setText(msg)
            icon = self.get_icon(record)
            if icon is not None:
                item.setIcon(icon)
            self.list_widget.update()
            self.list_widget.scrollToBottom()
            self.flush()
        except Exception:
            self.handleError(record)

# Urllib3 floods the log with messages about HTTP connections
# being established :/
lowered_loggers = ["urllib3.connectionpool", "requests.packages.urllib3.connectionpool"]
lowered_regexps = [re.compile("Starting new HTTP connection")]

class LogFilter(logging.Filter):
    def __init__(self, name='', lowered_regexps=None):
        logging.Filter.__init__(self, name)
        self.lowered_regexps = lowered_regexps

    def filter(self, record):
        if not logging.Filter.filter(self, record):
            return False
        if any(r.match(record.msg) is not None for r in self.lowered_regexps):
            return False
        return True

