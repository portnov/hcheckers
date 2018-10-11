
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


