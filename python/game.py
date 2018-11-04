
# REST client

from os.path import join
import requests
from threading import Thread, Lock

from common import *

class RequestError(Exception):
    pass

class MoveRequest(Thread):
    def __init__(self, owner, url, rq):
        Thread.__init__(self)
        self.owner = owner
        self.url = url
        self.rq = rq

    def run(self):
        try:
            self.owner.move_lock.acquire()
            rs = requests.post(self.url, json=self.rq)
            self.owner.process_response(rs)
            result = rs.json()
            print(result)
            self.owner.last_move_result = Game.parse_board(result["response"]), result["messages"]
        finally:
            self.owner.move_lock.release()

class AI(object):
    def __init__(self, **kwargs):
        self.title = "Default AI"
        self.depth = 2
        self.max_combination_depth = 6
        self.start_depth = None
        self.load = True
        self.store = False
        self.threads = 4
        self.update_cache_max_depth = 10
        self.update_cache_max_pieces = 0
        self.use_cache_max_depth = 10
        self.use_cache_max_pieces = 0

        for key in kwargs:
            setattr(self, key, kwargs[key])

    @classmethod
    def from_settings(cls, settings):
        ai = AI()
        ai.title = settings.value("title")
        ai.depth = settings.value("depth", type=int)
        ai.max_combination_depth = settings.value("max_combination_depth", type=int)
        ai.start_depth = settings.value("start_depth", type=int)
        ai.threads = settings.value("threads", type=int)
        ai.load = settings.value("load", type=bool)
        ai.store = settings.value("store", type=bool)
        return ai
    
    @classmethod
    def list_from_settings(cls, settings):
        result = []
        size = settings.beginReadArray("AI")
        if size is None or size == 0:
            result = [AI()]
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
        settings.setValue("threads", self.threads)
        settings.setValue("load", self.load)
        settings.setValue("store", self.store)

    def params(self):
        return {
            "depth": self.depth,
            "start_depth": self.start_depth,
            "max_combination_depth" : self.max_combination_depth,
            "load": self.load,
            "store" : self.store,
            "threads": self.threads,
            "update_cache_max_depth": self.update_cache_max_depth,
            "update_cache_max_pieces": self.update_cache_max_pieces,
            "use_cache_max_depth": self.use_cache_max_depth,
            "use_cache_max_pieces": self.use_cache_max_pieces
        }

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

class Game(object):
    def __init__(self, url=None):
        if url is None:
            url = "http://localhost:3000"
        self.base_url = url
        self.game_id = None
        self.user_name = None
        self.user_side = None
        self.ai_side = None
        self.last_move_result = None
        self.move_thread = None
        self.move_lock = Lock()

    def process_response(self, rs):
        if rs.status_code != requests.codes.ok:
            print(rs.text)
            raise RequestError(str(rs))

    def get_games(self, rules=None):
        if rules is not None:
            url = join(self.base_url, "lobby", rules)
        else:
            url = join(self.base_url, "lobby")
        rs = requests.get(url)
        self.process_response(rs)
        result = rs.json()["response"]
        return result

    def new_game(self, rules, board=None, fen_path=None):
        url = join(self.base_url, "game", "new")
        rq = {"rules": rules}
        if board is not None:
            rq["board"] = board
        elif fen_path is not None:
            fen_text = open(fen_path).read()
            rq["fen"] = fen_text
        rs = requests.post(url, json=rq)
        self.process_response(rs)
        result = rs.json()
        self.game_id = result["response"]["id"]
        return self.game_id

    def get_notation(self, rules):
        url = join(self.base_url, "notation", rules)
        rs = requests.get(url)
        self.process_response(rs)
        result = rs.json()["response"]
        return result["size"], result["notation"]

    def register_user(self, name, side):
        self.user_name = name
        self.user_side = side
        url = join(self.base_url, "game", self.game_id, "attach", name, str(side))
        rs = requests.post(url)
        self.process_response(rs)
        result = rs.json()

    def attach_ai(self, side, ai):
        self.ai_side = side
        url = join(self.base_url, "game", self.game_id, "attach", "ai", str(side))
        rq = {"ai": "default", "params": ai.params()}
        rs = requests.post(url, json=rq)
        self.process_response(rs)
        result = rs.json()

    def run_game(self):
        url = join(self.base_url, "game", self.game_id, "run")
        rs = requests.post(url)
        self.process_response(rs)
        result = rs.json()

    def start_new_game(self, user_name, rules="russian", board=None, fen_path=None, user_turn_first=True, ai=None):
        if ai is None:
            ai = AI()
        self.new_game(rules, board=board, fen_path=fen_path)
        if user_turn_first:
            self.register_user(user_name, FIRST)
            self.attach_ai(SECOND, ai)
        else:
            self.attach_ai(FIRST, ai)
            self.register_user(user_name, SECOND)
        self.run_game()

    def get_state(self):
        url = join(self.base_url, "game", self.game_id, "state")
        rs = requests.get(url)
        self.process_response(rs)
        result = rs.json()
        return result["response"]
    
    def poll(self):
        url = join(self.base_url, "poll", self.user_name)
        rs = requests.get(url)
        self.process_response(rs)
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
        rs = requests.get(url)
        self.process_response(rs)
        return rs.text
    
    def get_possible_moves(self, field=None):
        url = join(self.base_url, "game", self.game_id, "moves", self.user_name)
        rs = requests.get(url)
        self.process_response(rs)
        result = rs.json()
        moves = [Move.fromJson(item) for item in result["response"]]
        if field is None:
            return moves
        else:
            return [move for move in moves if move.from_field == field]

    def undo(self):
        url = join(self.base_url, "game", self.game_id, "undo", self.user_name)
        rs = requests.post(url)
        self.process_response(rs)
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
        print("Thread was started")

    def get_move_result(self):
        if self.move_thread is None:
            raise Exception("get_move_result() must be called after begin_move()")
        try:
            self.move_lock.acquire()
            result = self.last_move_result
            self.last_move_result = None
            self.move_thread = None
            return result
        finally:
            self.move_lock.release()

