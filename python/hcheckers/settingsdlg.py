
import json
import logging

from PyQt5.QtGui import QPainter, QPixmap, QIcon
from PyQt5.QtCore import QRect, QSize, Qt, QObject, QTimer, pyqtSignal, QSettings
from PyQt5.QtWidgets import QWidget, QDialog, QPushButton, QVBoxLayout, QHBoxLayout, QFormLayout, QLineEdit, QComboBox, QGroupBox, QCheckBox, QDialogButtonBox, QFileDialog, QListWidget, QListWidgetItem, QSpinBox, QToolBar, QAction, QTabWidget, QTextEdit, QTabWidget

from hcheckers.common import *
from hcheckers.game import AI, GameSettings
from hcheckers.theme import *

JSON_MASK = "JSON files (*.json)"

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
        self.tabs = QTabWidget(self)

        general = QWidget(self.tabs)
        layout = QFormLayout()
        general.setLayout(layout)

        def make_spinbox(title, low, high, tab):
            widget = QSpinBox(tab)
            widget.setRange(low, high)
            widget.valueChanged.connect(self.edited)
            tab.layout().addRow(title, widget)
            return widget

        self.title = MandatoryField(_("Title"), QLineEdit(general))
        self.title.add_to_form(layout)
        self.title.widget.editingFinished.connect(self.edited)

        self.depth = make_spinbox(_("Default depth (half-steps)"), 0, 20, general)
        self.start_depth = make_spinbox(_("Minimum depth"), 0, 20, general)
        self.max_combination_depth = make_spinbox(_("Forced mode depth"), 0, 24, general)
        self.dynamic_depth = make_spinbox(_("Static search mode threshold"), 0, 24, general)

        self.deeper_if_bad = QCheckBox(general)
        self.deeper_if_bad.stateChanged.connect(self.edited)
        layout.addRow(_("Think better if situation seem bad"), self.deeper_if_bad)

        self.moves_bound_low = make_spinbox(_("`Few moves' mode bound"), 1, 5, general)
        self.moves_bound_high = make_spinbox(_("`Too many moves' mode bound"), 5, 50, general)

        self.use_positional_score = QCheckBox(general)
        layout.addRow(_("Use positional score"), self.use_positional_score)
        self.use_positional_score.stateChanged.connect(self.edited)

        self.use_timeout = QCheckBox(general)
        layout.addRow(_("Continue thinking while there is time"), self.use_timeout)
        self.use_timeout.stateChanged.connect(self.edited)
        self.use_timeout.stateChanged.connect(self._on_use_timeout)

        self.timeout = make_spinbox(_("Timeout (seconds)"), 1, 120, general)
        self.timeout.setEnabled(False)

        self.random_opening_depth = make_spinbox(_("Random opening depth"), 1, 5, general)
        self.random_opening_depth.setToolTip(_("Number of first moves to be considered as opening; during these moves, AI will select it's move randomly from several best options"))
        self.random_opening_options = make_spinbox(_("Random opening options"), 1, 5, general)
        self.random_opening_options.setToolTip(_("From how many best options to select during the opening"))

        self.accept_draw = QComboBox(self)
        self.accept_draw.addItem(_("Always accept"), ALWAYS_ACCEPT)
        self.accept_draw.addItem(_("Always decline"), ALWAYS_DECLINE)
        self.accept_draw.addItem(_("Accept if AI is losing"), ACCEPT_IF_LOSING)
        self.accept_draw.currentIndexChanged.connect(self.edited)

        layout.addRow(_("Accept draws"), self.accept_draw)

        self.tabs.addTab(general, _("General"))

        evaluator = QWidget(self.tabs)
        layout = QFormLayout()
        evaluator.setLayout(layout)

        self.mobility_weight = make_spinbox(_("Mobility"), -500, 500, evaluator)
        self.backyard_weight = make_spinbox(_("Back row"), -100, 100, evaluator)
        self.center_weight = make_spinbox(_("Center"), -100, 100, evaluator)
        self.opposite_side_weight = make_spinbox(_("Opposite side"), -100, 100, evaluator)
        self.backed_weight = make_spinbox(_("Backed"), -100, 100, evaluator)
        self.asymetry_weight = make_spinbox(_("Asymetry"), -100, 100, evaluator)
        self.pre_king_weight = make_spinbox(_("Pre-king"), 1, 100, evaluator)
        self.king_coef = make_spinbox(_("King"), 1, 100, evaluator)
        self.attacked_man_coef = make_spinbox(_("Attacked man"), -300, 300, evaluator)
        self.attacked_king_coef = make_spinbox(_("Attacked king"), -300, 300, evaluator)

        self.tabs.addTab(evaluator, _("Board evaluation"))

        extra = QWidget(self.tabs)
        layout = QVBoxLayout()
        extra.setLayout(layout)

        self.extra = QTextEdit(general)
        self.extra.textChanged.connect(self.edited)
        layout.addWidget(self.extra)
        self.tabs.addTab(extra, _("Extra options"))

        layout = QVBoxLayout()
        layout.addWidget(self.tabs)

        hbox = QHBoxLayout()

        save = QPushButton(_("Save..."), self)
        save.setIcon(QIcon.fromTheme("document-save"))
        save.setToolTip(_("Save AI settings to JSON file"))
        save.clicked.connect(self._on_save)
        hbox.addWidget(save)

        load = QPushButton(_("Load..."), self)
        load.setIcon(QIcon.fromTheme("document-open"))
        load.setToolTip(_("Load AI settings from JSON file"))
        load.clicked.connect(self._on_load)
        hbox.addWidget(load)

        layout.addLayout(hbox)

        self.setLayout(layout)

    def _on_use_timeout(self):
        use = self.use_timeout.checkState() == Qt.Checked
        self.timeout.setEnabled(use)

    def _on_save(self):
        path, mask = QFileDialog.getSaveFileName(self, _("Save file"), ".", JSON_MASK)
        if path:
            ai = self.get_ai()
            json_data = ai.params()
            with open(path, 'w') as f:
                f.write(json.dumps(json_data))

    def _on_load(self):
        path, mask = QFileDialog.getOpenFileName(self, _("Load file"), ".", JSON_MASK)
        if path:
            try:
                with open(path) as f:
                    text = f.read()
                    json_data = json.loads(text)
                    ai = AI()
                    ai.title = self.get_ai().title
                    ai.load_json(json_data)
                    self.set_ai(ai)
            except Exception as e:
                logging.exception(e)

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
        self.random_opening_depth.setValue(ai.random_opening_depth)
        self.random_opening_options.setValue(ai.random_opening_options)

        self.mobility_weight.setValue(ai.mobility_weight)
        self.backyard_weight.setValue(ai.backyard_weight)
        self.center_weight.setValue(ai.center_weight)
        self.opposite_side_weight.setValue(ai.opposite_side_weight)
        self.backed_weight.setValue(ai.backed_weight)
        self.asymetry_weight.setValue(ai.asymetry_weight)
        self.pre_king_weight.setValue(ai.pre_king_weight)
        self.king_coef.setValue(ai.king_coef)
        self.attacked_man_coef.setValue(- ai.attacked_man_coef)
        self.attacked_king_coef.setValue(- ai.attacked_king_coef)

        policy = ai.accept_draw
        policy_idx = self.accept_draw.findData(policy)
        self.accept_draw.setCurrentIndex(policy_idx)

        self.extra.setText("" if ai.extra is None else ai.extra)

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
        ai.random_opening_depth = self.random_opening_depth.value()
        ai.random_opening_options = self.random_opening_options.value()

        ai.mobility_weight = self.mobility_weight.value()
        ai.backyard_weight = self.backyard_weight.value()
        ai.center_weight = self.center_weight.value()
        ai.opposite_side_weight = self.opposite_side_weight.value()
        ai.backed_weight = self.backed_weight.value()
        ai.asymetry_weight = self.asymetry_weight.value()
        ai.pre_king_weight = self.pre_king_weight.value()
        ai.king_coef = self.king_coef.value()
        ai.attacked_man_coef = - self.attacked_man_coef.value()
        ai.attacked_king_coef = - self.attacked_king_coef.value()

        ai.accept_draw = self.accept_draw.currentData()

        ai.extra = self.extra.toPlainText()
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

        self.copy_ai = QAction(_("Duplicate AI preset"), self)
        self.copy_ai.setIcon(QIcon.fromTheme("edit-copy"))
        self.copy_ai.triggered.connect(self._on_copy)
        self.toolbar.addAction(self.copy_ai)

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

    def _on_copy(self):
        ai = self.selector.get_selected_ai().copy()
        ai.title = _("Copy of {}").format(ai.title)
        self.selector.add_ai(ai)
        self.selector.setCurrentRow(self.selector.count()-1)

    def _on_del(self):
        self.selector.delete_selected_ai()

class ViewSettingsPage(QWidget):
    def __init__(self, share_dir, parent=None):
        QWidget.__init__(self, parent)
        layout = QFormLayout()
        self.show_notation = QCheckBox(self)
        layout.addRow(_("Show fields notation on the board"), self.show_notation)
        self.show_border = QCheckBox(self)
        layout.addRow(_("Show board borders with notation"), self.show_border)
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

    def get_enable_sound(self):
        return self.enable_sound.checkState() == Qt.Checked

    def load(self, settings):
        show_notation = settings.value("show_notation", type=bool)
        self.show_notation.setCheckState(Qt.Checked if show_notation else Qt.Unchecked)

        show_border = settings.value("show_border", False, type=bool)
        self.show_border.setCheckState(Qt.Checked if show_border else Qt.Unchecked)

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
        settings.setValue("show_border", self.show_border.checkState() == Qt.Checked)
        settings.setValue("show_possible_moves", self.show_possible_moves.checkState() == Qt.Checked)
        settings.setValue("theme", self.theme.currentData())
        settings.setValue("enable_sound", self.get_enable_sound())

class GeneralPage(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        layout = QFormLayout()

        self.server_url = MandatoryField(_("Server URL"), QLineEdit(self))
        self.server_url.add_to_form(layout)

        self.use_local_server = QCheckBox(self)
        layout.addRow(_("Use local server"), self.use_local_server)

        self.local_server_path = MandatoryField(_("Local server start command"), QLineEdit(self))
        self.local_server_path.add_to_form(layout)

        self.use_local_server.stateChanged.connect(self._on_use_local_server)

        self.log_level= QComboBox(self)
        self.log_level.addItem(_("Debug"), logging.DEBUG)
        self.log_level.addItem(_("Information"), logging.INFO)
        self.log_level.addItem(_("Warning"), logging.WARN)
        self.log_level.addItem(_("Error"), logging.ERROR)

        layout.addRow(_("Logging level"), self.log_level)

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

        level = settings.value("log_level", logging.INFO, type=int)
        level_idx = self.log_level.findData(level)
        self.log_level.setCurrentIndex(level_idx)

    def save(self, settings):
        settings.setValue("server_url", self.server_url.widget.text())
        use_local_server = self.use_local_server.checkState() == Qt.Checked
        settings.setValue("use_local_server", use_local_server)
        settings.setValue("local_server_path", self.local_server_path.widget.text())
        level = self.log_level.currentData()
        settings.setValue("log_level", level)

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
        self.setWindowTitle(_("HCheckers settings"))

    saved = pyqtSignal()

    def get_ok_button(self):
        return self.ok_button

    def get_show_notation(self):
        return self.view.show_notation.checkState() == Qt.Checked

    def get_show_border(self):
        return self.view.show_border.checkState() == Qt.Checked

    def get_show_possible_moves(self):
        return self.view.show_possible_moves.checkState() == Qt.Checked

    def get_theme(self):
        return self.view.get_theme()

    def get_enable_sound(self):
        return self.view.get_enable_sound()
    
    def _on_accept(self):
        self.general.save(self.settings)
        self.view.save(self.settings)
        self.ais.save(self.settings)
        self.saved.emit()
        self.accept()

