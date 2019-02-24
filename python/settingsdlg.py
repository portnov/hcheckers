
from PyQt5.QtGui import QPainter, QPixmap, QIcon
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QWidget, QDialog, QPushButton, QVBoxLayout, QHBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, QDialogButtonBox, QFileDialog, QListWidget, QListWidgetItem, QSpinBox, QToolBar, QAction, QTabWidget

from common import *
from game import AI, GameSettings
from theme import *

class AiListWidget(QListWidget):
    def __init__(self, parent=None):
        QListWidget.__init__(self, parent)
        self.ais = []

    def add_ai(self, ai=None):
        if ai is None:
            ai = AI()
        idx = len(self.ais)
        self.ais.append(ai)
        title = ai.title
        if not title:
            title = "Untitled"
        item = QListWidgetItem(self)
        item.setText(title)
        self.update()

    def load_ais(self, settings):
        for ai in AI.list_from_settings(settings):
            self.add_ai(ai)

    def save_ais(self, settings):
        settings.beginWriteArray("AI")
        for idx, ai in enumerate(self.ais):
            settings.setArrayIndex(idx)
            ai.to_settings(settings)
        settings.endArray()

    def get_selected_ai(self):
        idx = self.currentRow()
        return self.ais[idx]
    
    def set_selected_ai(self, ai):
        idx = self.currentRow()
        if idx < 0:
            return
        self.ais[idx] = ai
        item = self.item(idx)
        if item is not None:
            item.setText(ai.title)

    def delete_selected_ai(self):
        idx = self.currentRow()
        if idx < 0:
            return
        self.ais.remove(self.ais[idx])
        self.takeItem(idx)

class AiEditorWidget(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self._setup()

    edited = pyqtSignal()

    def _setup(self):
        layout = QFormLayout()
        self.title = MandatoryField(_("Title"), QLineEdit(self))
        self.title.add_to_form(layout)
        self.title.widget.editingFinished.connect(self.edited)

        self.depth = QSpinBox(self)
        self.depth.setRange(0, 20)
        self.depth.valueChanged.connect(self.edited)
        layout.addRow(_("Default depth (half-steps)"), self.depth)

        self.start_depth = QSpinBox(self)
        self.start_depth.setRange(0, 20)
        self.start_depth.valueChanged.connect(self.edited)
        layout.addRow(_("Minimum depth"), self.start_depth)

        self.max_combination_depth = QSpinBox(self)
        self.max_combination_depth.setRange(0, 24)
        self.max_combination_depth.valueChanged.connect(self.edited)
        layout.addRow(_("Forced mode depth"), self.max_combination_depth)

        self.dynamic_depth = QSpinBox(self)
        self.dynamic_depth.setRange(0, 24)
        self.dynamic_depth.valueChanged.connect(self.edited)
        layout.addRow(_("Static search mode threshold"), self.dynamic_depth)

        self.deeper_if_bad = QCheckBox(self)
        self.deeper_if_bad.stateChanged.connect(self.edited)
        layout.addRow(_("Think better if situation seem bad"), self.deeper_if_bad)

        self.moves_bound_low = QSpinBox(self)
        self.moves_bound_low.setRange(1, 5)
        self.moves_bound_low.valueChanged.connect(self.edited)
        layout.addRow(_("`Few moves' mode bound"), self.moves_bound_low)

        self.moves_bound_high = QSpinBox(self)
        self.moves_bound_high.setRange(5, 50)
        self.moves_bound_high.valueChanged.connect(self.edited)
        layout.addRow(_("`Too many moves' mode bound"), self.moves_bound_high)

        self.use_positional_score = QCheckBox(self)
        layout.addRow(_("Use positional score"), self.use_positional_score)
        self.use_positional_score.stateChanged.connect(self.edited)

        self.use_timeout = QCheckBox(self)
        layout.addRow(_("Continue thinking while there is time"), self.use_timeout)
        self.use_timeout.stateChanged.connect(self.edited)
        self.use_timeout.stateChanged.connect(self._on_use_timeout)

        self.timeout = QSpinBox(self)
        self.timeout.setRange(1, 120)
        self.timeout.setEnabled(False)
        self.timeout.valueChanged.connect(self.edited)
        layout.addRow(_("Timeout (seconds)"), self.timeout)

        self.setLayout(layout)

    def _on_use_timeout(self):
        use = self.use_timeout.checkState() == Qt.Checked
        self.timeout.setEnabled(use)

    def set_ai(self, ai):
        self.title.widget.setText(ai.title)
        self.depth.setValue(ai.depth)
        if ai.start_depth is not None:
            self.start_depth.setValue(ai.start_depth)
        self.max_combination_depth.setValue(ai.max_combination_depth)
        self.dynamic_depth.setValue(ai.dynamic_depth)
        self.deeper_if_bad.setCheckState(Qt.Checked if ai.deeper_if_bad else Qt.Unchecked)
        self.moves_bound_low.setValue(ai.moves_bound_low)
        self.moves_bound_high.setValue(ai.moves_bound_high)
        self.use_positional_score.setCheckState(Qt.Checked if ai.use_positional_score else Qt.Unchecked)
        self.use_timeout.setCheckState(Qt.Checked if ai.use_timeout else Qt.Unchecked)
        self.timeout.setValue(1 if ai.timeout is None else ai.timeout)

    def get_ai(self):
        ai = AI()
        ai.title = self.title.widget.text()
        ai.depth = self.depth.value()
        ai.start_depth = self.start_depth.value()
        ai.max_combination_depth = self.max_combination_depth.value()
        ai.dynamic_depth = self.dynamic_depth.value()
        ai.deeper_if_bad = self.deeper_if_bad.checkState() == Qt.Checked
        ai.moves_bound_low = self.moves_bound_low.value()
        ai.moves_bound_high = self.moves_bound_high.value()
        ai.use_positional_score = self.use_positional_score.checkState() == Qt.Checked
        ai.use_timeout = self.use_timeout.checkState() == Qt.Checked
        ai.timeout = self.timeout.value()
        return ai

class AiPresetsPage(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.selector = AiListWidget(self)
        self.editor = AiEditorWidget(self)
        layout = QHBoxLayout()
        vbox = QVBoxLayout()
        self.toolbar = QToolBar(self)
        self.add_ai = QAction(_("Add AI preset"), self)
        self.add_ai.setIcon(QIcon.fromTheme("list-add"))
        self.add_ai.triggered.connect(self._on_add)
        self.toolbar.addAction(self.add_ai)
        self.del_ai = QAction(_("Delete AI preset"), self)
        self.del_ai.setIcon(QIcon.fromTheme("list-remove"))
        self.del_ai.triggered.connect(self._on_del)
        self.toolbar.addAction(self.del_ai)
        vbox.addWidget(self.toolbar)
        vbox.addWidget(self.selector)
        layout.addLayout(vbox)
        layout.addWidget(self.editor)
        self.setLayout(layout)
        self.setup_mode = True
        self.selector.currentRowChanged.connect(self._on_ai_selected)
        self.editor.edited.connect(self._on_ai_edited)

    def load(self, settings):
        self.selector.load_ais(settings)
        self.selector.setCurrentRow(0)
        self.editor.set_ai(self.selector.ais[0])
        self.setup_mode = False

    def save(self, settings):
        self.selector.save_ais(settings)

    def _on_ai_edited(self):
        if not self.setup_mode:
            self.selector.set_selected_ai(self.editor.get_ai())
    
    def _on_ai_selected(self, idx):
        self.editor.set_ai(self.selector.get_selected_ai())

    def _on_add(self):
        self.selector.add_ai()
        self.selector.setCurrentRow(self.selector.count()-1)

    def _on_del(self):
        self.selector.delete_selected_ai()

class ViewSettingsPage(QWidget):
    def __init__(self, share_dir, parent=None):
        QWidget.__init__(self, parent)
        layout = QFormLayout()
        self.show_notation = QCheckBox(self)
        layout.addRow(_("Show fields notation"), self.show_notation)
        self.show_possible_moves = QCheckBox(self)
        layout.addRow(_("Show possible moves"), self.show_possible_moves)
        self.theme = QComboBox(self)
        self.themes = dict()
        for theme in Theme.list_themes(share_dir):
            self.themes[theme.id] = theme
            self.theme.addItem(theme.name, theme.id)
        layout.addRow(_("Theme"), self.theme)

        self.enable_sound = QCheckBox(self)
        layout.addRow(_("Enable sounds"), self.enable_sound)

        self.setLayout(layout)

    def get_theme(self):
        theme_name = self.theme.currentData()
        return self.themes[theme_name]

    def load(self, settings):
        show_notation = settings.value("show_notation", type=bool)
        self.show_notation.setCheckState(Qt.Checked if show_notation else Qt.Unchecked)

        show_possible_moves = settings.value("show_possible_moves", type=bool)
        self.show_possible_moves.setCheckState(Qt.Checked if show_possible_moves else Qt.Unchecked)

        theme = settings.value("theme")
        theme_idx = self.theme.findData(theme)
        if theme_idx < 0:
            theme_idx = self.theme.findText("default")
        if theme_idx >= 0:
            self.theme.setCurrentIndex(theme_idx)

        enable_sound = settings.value("enable_sound", type=bool)
        self.enable_sound.setCheckState(Qt.Checked if enable_sound else Qt.Unchecked)

    def save(self, settings):
        settings.setValue("show_notation", self.show_notation.checkState() == Qt.Checked)
        settings.setValue("show_possible_moves", self.show_possible_moves.checkState() == Qt.Checked)
        settings.setValue("theme", self.theme.currentData())
        settings.setValue("enable_sound", self.enable_sound.checkState() == Qt.Checked)

class GeneralPage(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        layout = QFormLayout()

        self.server_url = MandatoryField(_("Server URL"), QLineEdit(self))
        self.server_url.add_to_form(layout)

        self.use_local_server = QCheckBox(self)
        layout.addRow(_("Use local server"), self.use_local_server)

        self.local_server_path = MandatoryField(_("Local server executable path"), QLineEdit(self))
        self.local_server_path.add_to_form(layout)

        self.use_local_server.stateChanged.connect(self._on_use_local_server)

        self.setLayout(layout)

    def _on_use_local_server(self):
        use = self.use_local_server.checkState() == Qt.Checked
        self.local_server_path.widget.setEnabled(use)
        self.local_server_path.is_mandatory = use

    def load(self, settings):
        url = settings.value("server_url", DEFAULT_SERVER_URL)
        self.server_url.widget.setText(url)

        use_local_server = settings.value("use_local_server", type=bool)
        self.use_local_server.setCheckState(Qt.Checked if use_local_server else Qt.Unchecked)

        path = settings.value("local_server_path", "hcheckersd --local")
        self.local_server_path.widget.setText(path)
        self.local_server_path.widget.setEnabled(use_local_server)
        self.local_server_path.widget.is_mandatory = use_local_server

    def save(self, settings):
        settings.setValue("server_url", self.server_url.widget.text())
        use_local_server = self.use_local_server.checkState() == Qt.Checked
        settings.setValue("use_local_server", use_local_server)
        settings.setValue("local_server_path", self.local_server_path.widget.text())

class SettingsDialog(DialogBase):
    def __init__(self, settings, share_dir, parent=None):
        QDialog.__init__(self, parent)
        self.settings = settings
        self.share_dir = share_dir
        layout = QVBoxLayout()
        self.tabs = QTabWidget(self)
        self.general = GeneralPage(self)
        self.tabs.addTab(self.general, _("General"))
        self.view = ViewSettingsPage(share_dir, self)
        self.tabs.addTab(self.view, _("View Settings"))
        self.ais = AiPresetsPage(self)
        self.tabs.addTab(self.ais, _("AI Presets"))
        layout.addWidget(self.tabs)
        buttons = QDialogButtonBox(
                    QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
                    Qt.Horizontal, self)
        buttons.accepted.connect(self._on_accept)
        buttons.rejected.connect(self.reject)
        self.ok_button = buttons.button(QDialogButtonBox.Ok)
        layout.addWidget(buttons)
        self.setLayout(layout)
        self.general.load(settings)
        self.view.load(settings)
        self.ais.load(settings)

    def get_ok_button(self):
        return self.ok_button

    def get_show_notation(self):
        return self.view.show_notation.checkState() == Qt.Checked

    def get_show_possible_moves(self):
        return self.view.show_possible_moves.checkState() == Qt.Checked

    def get_theme(self):
        return self.view.get_theme()
    
    def _on_accept(self):
        self.general.save(self.settings)
        self.view.save(self.settings)
        self.ais.save(self.settings)
        self.accept()

