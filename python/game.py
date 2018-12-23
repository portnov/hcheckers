
# REST client

from os.path import join
import requests
import logging
from threading import Thread, Lock

from common import *

class MoveRequest(Thread):
    def __init__(self, owner, url, rq):
        Thread.__init__(self)
        self.owner = owner
        self.url = url
        self.rq = rq

    def run(self):
        try:
            self.owner.move_lock.acquire()
            self.owner.move_exception = None
            rs = self.owner.post(self.url, json=self.rq)
            result = rs.json()
            #logging.debug(result)
            self.owner.last_move_result = Game.parse_board(result["response"]), result["messages"]
        except Exception as e:
            self.owner.move_exception = e
        finally:
            self.owner.move_lock.release()

class AI(object):
    def __init__(self, **kwargs):
        self.title = "Default AI"
        self.depth = 2
        self.max_combination_depth = 6
        self.start_depth = None
        self.use_positional_score = True
        self.timeout = None
        self.use_timeout = False

        for key in kwargs:
            setattr(self, key, kwargs[key])

    @classmethod
    def from_settings(cls, settings):
        ai = AI()
        ai.title = settings.value("title")
        ai.depth = settings.value("depth", type=int)
        ai.max_combination_depth = settings.value("max_combination_depth", type=int)
        ai.start_depth = settings.value("start_depth", type=int)
        ai.use_positional_score = settings.value("use_positional_score", type=bool)
        ai.use_timeout = settings.value("use_timeout", type=bool)
        ai.timeout = settings.value("timeout", type=int)
        return ai
    
    @classmethod
    def list_from_settings(cls, settings):
        result = []
        size = settings.beginReadArray("AI")
        if size is None or size == 0:
            result = default_ais
        else:
            for idx in range(size):
                settings.setArrayIndex(idx)
                ai = AI.from_settings(settings)
                result.append(ai)
        settings.endArray()
        return result

    
    def to_settings(self, settings):
        settings.setValue("title", self.title)
        settings.setValue("depth", self.depth)
        settings.setValue("max_combination_depth", self.max_combination_depth)
        settings.setValue("start_depth", self.start_depth)
        settings.setValue("use_positional_score", self.use_positional_score)
        settings.setValue("use_timeout", self.use_timeout)
        settings.setValue("timeout", self.timeout)

    def params(self):
        return {
            "depth": self.depth,
            "start_depth": self.start_depth,
            "max_combination_depth": self.max_combination_depth,
            "use_positional_score": self.use_positional_score,
            "time": self.timeout if self.use_timeout else None
        }

default_ais = [
        AI(title=_("Beginner"), depth=2, max_combination_depth=6),
        AI(title=_("Novice"), depth=4, max_combination_depth=4),
        AI(title=_("Average"), depth=6, max_combination_depth=4),
        AI(title=_("Good"), depth=6, max_combination_depth=9),
        AI(title=_("Master"), depth=7, max_combination_depth=9)
    ]

class GameSettings(object):
    def __init__(self):
        self.user_name = None
        self.ai = None
        self.rules = None
        self.run_now = False
        self.url = None
        self.user_turn_first = True
        self.board_setup = False
        self.action = None
        self.fen_path = None
        self.pdn_path = None

class Game(object):
    def __init__(self, url=None):
        if url is None:
            url = DEFAULT_SERVER_URL
        self.base_url = url
        self.game_id = None
        self.user_name = None
        self.user_side = None
        self.ai_side = None
        self.last_move_result = None
        self.rules = None
        self.move_thread = None
        self.move_lock = Lock()
        self.move_exception = None

    def process_response(self, rs, action=None):
        if rs is None:
            return
        if rs.status_code != requests.codes.ok:
            logging.warning(rs.url)
            logging.warning(rs.text)
            raise RequestError(rs)
    
    def get(self, url, *args, **kwargs):
        rs = requests.get(url, *args, **kwargs)
        self.process_response(rs)
        return rs

    def post(self, url, *args, **kwargs):
        rs = requests.post(url, *args, **kwargs)
        self.process_response(rs)
        return rs

    def get_games(self, rules=None):
        if rules is not None:
            url = join(self.base_url, "lobby", rules)
        else:
            url = join(self.base_url, "lobby")
        rs = self.get(url)
        result = rs.json()["response"]
        return result

    def new_game(self, rules, board=None, fen_path=None, pdn_path=None):
        url = join(self.base_url, "game", "new")
        rq = {"rules": rules}
        if board is not None:
            rq["board"] = board
        elif fen_path is not None:
            fen_text = open(fen_path).read()
            rq["fen"] = fen_text
        elif pdn_path is not None:
            pdn_text = open(pdn_path).read()
            rq["pdn"] = pdn_text
        rs = self.post(url, json=rq)
        result = rs.json()
        self.game_id = result["response"]["id"]
        self.rules = rules
        return self.game_id

    def get_notation(self, rules):
        url = join(self.base_url, "notation", rules)
        rs = self.get(url)
        result = rs.json()["response"]
        invert = result["orientation"] == 'SecondAtBottom'
        return result["size"], invert, result["notation"]
    
    def get_invert_colors(self, rules):
        _size, invert, _notation = self.get_notation(rules)
        return invert

    def get_color_mapping(self, rules):
        """
        Return a tuple (side1, side2), where
        side1 is one who uses white pieces, and
        side2 is one who uses black pieces.
        """
        _size, invert, _notation = self.get_notation(rules)
        if invert:
            return SECOND, FIRST
        else:
            return FIRST, SECOND

    def get_colors(self, rules):
        invert = self.get_invert_colors(rules)
        if invert:
            return _("Black"), _("White")
        else:
            return _("White"), _("Black")

    def register_user(self, name, side):
        self.user_name = name
        self.user_side = side
        url = join(self.base_url, "game", self.game_id, "attach", name, str(side))
        rs = self.post(url)
        result = rs.json()

    def attach_ai(self, side, ai):
        self.ai_side = side
        url = join(self.base_url, "game", self.game_id, "attach", "ai", str(side))
        rq = {"ai": "default", "params": ai.params()}
        rs = self.post(url, json=rq)
        result = rs.json()

    def run_game(self):
        url = join(self.base_url, "game", self.game_id, "run")
        rs = self.post(url)
        result = rs.json()

    def start_new_game(self, user_name, rules="russian", board=None, fen_path=None, pdn_path=None, user_turn_first=True, ai=None):
        if ai is None:
            ai = AI()
        self.new_game(rules, board=board, fen_path=fen_path, pdn_path=pdn_path)
        if user_turn_first:
            self.register_user(user_name, FIRST)
            self.attach_ai(SECOND, ai)
        else:
            self.attach_ai(FIRST, ai)
            self.register_user(user_name, SECOND)
        self.run_game()

    def get_state(self):
        url = join(self.base_url, "game", self.game_id, "state")
        rs = self.get(url)
        result = rs.json()
        return result["response"]
    
    def get_history(self):
        if self.game_id is None:
            return None
        url = join(self.base_url, "game", self.game_id, "history")
        rs = self.get(url)
        result = rs.json()
        return result["response"]
    
    def poll(self):
        url = join(self.base_url, "poll", self.user_name)
        rs = self.get(url)
        result = rs.json()
        messages = result["response"]
        board = self.get_board()
        return board, messages

    @classmethod
    def parse_board(cls, json):
        board = dict()
        for label, value in json:
            board[Label.fromJson(label)] = Piece.fromJson(value)
        return board

    def get_board(self):
        state = self.get_state()
        if state is not None:
            board = Game.parse_board(state["board"])
            return board
    
    def get_fen(self):
        url = join(self.base_url, "game", self.game_id, "fen")
        rs = self.get(url)
        return rs.text
    
    def get_pdn(self):
        url = join(self.base_url, "game", self.game_id, "pdn")
        rs = self.get(url)
        return rs.text
    
    def get_possible_moves(self, field=None):
        url = join(self.base_url, "game", self.game_id, "moves", self.user_name)
        rs = self.get(url)
        result = rs.json()
        moves = [Move.fromJson(item) for item in result["response"]]
        if field is None:
            return moves
        else:
            return [move for move in moves if move.from_field == field]

    def undo(self):
        url = join(self.base_url, "game", self.game_id, "undo", self.user_name)
        rs = self.post(url)
        result = rs.json()
        board = Game.parse_board(result["response"])
        return board
    
    def begin_move(self, src, dst):
        if self.move_thread is not None:
            raise Exception("it seems get_move_result() was not called after previous begin_move()")
        url = join(self.base_url, "game", self.game_id, "move", self.user_name)
        rq = {"from": src.json(), "to": dst.json()}
        self.last_move_result = None
        self.move_thread = MoveRequest(self, url, rq)
        self.move_thread.start()
        logging.debug("Thread was started")

    def get_move_result(self):
        if self.move_thread is None:
            raise Exception("get_move_result() must be called after begin_move()")
        try:
            self.move_lock.acquire()
            if self.move_exception is not None:
                raise self.move_exception
            result = self.last_move_result
            self.last_move_result = None
            self.move_thread = None
            return result
        finally:
            self.move_lock.release()

