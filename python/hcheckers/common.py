
from requests.exceptions import ConnectionError
from PyQt5.QtWidgets import QDialog, QLineEdit

MAN = 1
KING = 2

FIRST = 1
SECOND = 2

WE_REQUESTED_DRAW = 1
DRAW_REQUESTED_FROM_US = 2

DEFAULT_SERVER_URL = "http://localhost:8864"

class Piece(object):
    def __init__(self, kind, side):
        self.kind = kind
        self.side = side

    def __str__(self):
        return "<{} {}>".format(self.kind, self.side)
    
    def inverted(self):
        piece = Piece(self.kind, (FIRST+SECOND) - self.side)
        return piece

    def json(self):
        kind = 'Man' if self.kind == MAN else 'King'
        side = 'First' if self.side == FIRST else 'Second'
        return {'kind': kind, 'side': side}

    @classmethod
    def fromJson(cls, json):
        piece = Piece(None,None)
        if json["kind"] == "Man":
            piece.kind = MAN
        else:
            piece.kind = KING

        if json["side"] == "First":
            piece.side = 1
        else:
            piece.side = 2

        return piece

class Label(object):
    def __init__(self, col, row):
        self.col = col
        self.row = row

    def __str__(self):
        return "[{},{}]".format(self.col, self.row)

    def __repr__(self):
        return "[{},{}]".format(self.col, self.row)

    def __hash__(self):
        return hash((self.col, self.row))

    def __eq__(self, that):
        if not isinstance(that, Label):
            raise Exception("Label.== to another class")
        return (self.col, self.row) == (that.col, that.row)

    def __ne__(self, that):
        return not (self == that)

    def json(self):
        return [self.col, self.row]

    @classmethod
    def fromJson(cls, json):
        label = Label(None, None)
        label.col = json[0]
        label.row = json[1]
        return label

class Step(object):
    def __init__(self):
        self.field = None
        self.capture = False
        self.promote = False

    def __str__(self):
        return "{}[capture={}][promote={}]".format(self.field, self.capture, self.promote)
    
    def json(self):
        json = dict()
        json["field"] = self.field.json()
        json["capture"] = self.capture
        json["promote"] = self.promote
        return json

    @classmethod
    def fromJson(cls, json):
        step = Step()
        step.field = Label.fromJson(json["field"])
        step.capture = json["capture"]
        step.promote = json["promote"]
        return step

class Move(object):
    def __init__(self):
        self.from_field = None
        self.steps = []

    def __str__(self):
        return "[{}] {}".format(self.from_field, self.steps)

    def json(self):
        json = dict()
        json["from"] = self.from_field
        json["steps"] = []
        for step in self.steps:
            json["steps"].append(step.json())
        return json

    @classmethod
    def fromJson(cls, json):
        move = Move()
        move.from_field = Label.fromJson(json["from"])
        for item in json["steps"]:
            step = Step.fromJson(item)
            move.steps.append(step)
        return move

class Message(object):
    pass

class OtherSideMove(Message):
    def __init__(self, board, move):
        self.board = board
        self.move = move

    def __str__(self):
        return _("Other side move: {}").format(self.board.show_move(self.move))


class UndoMessage(Message):
    def __str__(self):
        return _("Other side requested undo")

class GameResultMessage(Message):
    def __init__(self, result):
        self.result = result

    def __str__(self):
        return "Game result: {}".format(self.result)

class WaitingMove(Message):
    def __str__(self):
        return _("Wating for another side turn")

class DrawRequestedMessage(Message):
    def __str__(self):
        return _("Another side have offered a draw")

class DrawResponseMessage(Message):
    def __init__(self, result):
        self.result = result

    def __str__(self):
        if self.result:
            return _("Another side accepted the draw.")
        else:
            return _("Another side declined the draw.")

class RequestError(Exception):
    def __init__(self, rs):
        Exception.__init__(self, str(rs))
        self.rs = rs

def handling_error(method):
    def wrapped(self, *args, **kwargs):
        try:
            result = method(self, *args, **kwargs)
            return result
        except RequestError as e:
            self._handle_game_error(e.rs)
        except ConnectionError as e:
            self._handle_connection_error(e.request.url, e)
    wrapped.__name__ = method.__name__
    return wrapped

class MandatoryField(object):
    def __init__(self, label, widget):
        self.widget = widget
        self.label = label
        self._is_mandatory = True
        self._dialog = DialogBase.find(widget)
        self.layout = None
        if isinstance(widget, QLineEdit):
            widget.textEdited.connect(self._on_edit)

    def _on_edit(self, text):
        empty = text is None or text == ""
        self._dialog.get_ok_button().setEnabled(not empty)
        self.layout.labelForField(self.widget).font().setBold(empty)

    def add_to_form(self, layout):
        layout.addRow(self.label, self.widget)
        self.layout = layout

    def get_is_mandatory(self):
        return self._is_mandatory

    def set_is_mandatory(self, value):
        self._is_mandatory = value

    is_mandatory = property(get_is_mandatory, set_is_mandatory)

class DialogBase(QDialog):
    def __init__(self, parent=None):
        QDialog.__init__(self, parent)

    @classmethod
    def find(cls, widget):
        if widget is None:
            return None
        if isinstance(widget, DialogBase):
            return widget
        return DialogBase.find(widget.parentWidget())

    def get_form_layout(self):
        raise Exception("the method is to be implemented")

    def get_ok_button(self):
        raise Exception("the method is to be implemented")

supported_rules = [
        ("russian", _("Russian draughts")),
        ("simple", _("Simple draughts")),
        ("english", _("English draughts")), 
        ("international", _("International draughts")),
        ("brazilian", _("Brazilian draughts")),
        ("canadian", _("Canadian draughts")),
        ("spancirety", _("Spancirety draughts")), 
        ("diagonal", _("Diagonal draughts"))
    ]

rules_dict = dict(supported_rules)

