
MAN = 1
KING = 2

FIRST = 1
SECOND = 2

class Piece(object):
    def __init__(self, kind, side):
        self.kind = kind
        self.side = side

    def __str__(self):
        return "<{} {}>".format(self.kind, self.side)

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


