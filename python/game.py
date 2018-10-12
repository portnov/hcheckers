
# REST client

from os.path import join
import requests

from common import *

class RequestError(Exception):
    pass

class Game(object):
    def __init__(self, url=None):
        if url is None:
            url = "http://localhost:3000"
        self.base_url = url
        self.game_id = None
        self.user_name = None
        self.user_side = None
        self.ai_side = None

    def process_response(self, rs):
        if rs.status_code != requests.codes.ok:
            print(rs)
            raise RequestError(str(rs))

    def new_game(self, rules, board=None):
        url = join(self.base_url, "game", "new")
        rq = {"rules": rules}
        if board is not None:
            rq["board"] = board
        rs = requests.post(url, json=rq)
        self.process_response(rs)
        result = rs.json()
        self.game_id = result["response"]["id"]
        return self.game_id

    def register_user(self, name, side):
        self.user_name = name
        self.user_side = side
        url = join(self.base_url, "game", self.game_id, "attach", name, str(side))
        rs = requests.post(url)
        self.process_response(rs)
        result = rs.json()

    def attach_ai(self, side, depth=2):
        self.ai_side = side
        url = join(self.base_url, "game", self.game_id, "attach", "ai", str(side))
        rq = {"ai": "default", "params": {"depth": depth}}
        rs = requests.post(url, json=rq)
        self.process_response(rs)
        result = rs.json()

    def run_game(self):
        url = join(self.base_url, "game", self.game_id, "run")
        rs = requests.post(url)
        self.process_response(rs)
        result = rs.json()

    def start_new_game(self, user_name, rules="russian", board=None, user_turn_first=True, ai_depth=2):
        self.new_game(rules, board)
        if user_turn_first:
            self.register_user(user_name, 1)
            self.attach_ai(2, ai_depth)
        else:
            self.attach_ai(1, ai_depth)
            self.register_user(user_name, 2)
        self.run_game()

    def get_state(self):
        url = join(self.base_url, "game", self.game_id, "state")
        rs = requests.get(url)
        self.process_response(rs)
        result = rs.json()
        return result["response"]

    @classmethod
    def parse_board(cls, json):
        board = dict()
        for label, value in json:
            board[label] = Piece.fromJson(value)
        return board

    def get_board(self):
        state = self.get_state()
        if state is not None:
            board = Game.parse_board(state["board"])
            return board
    
    def get_possible_moves(self, field=None):
        url = join(self.base_url, "game", self.game_id, "moves", self.user_name)
        rs = requests.get(url)
        self.process_response(rs)
        result = rs.json()
        if field is None:
            return result["response"]
        else:
            moves = result["response"]
            return [move for move in moves if move["from"] == field]
    
    def move(self, src, dst):
        url = join(self.base_url, "game", self.game_id, "move", self.user_name)
        rq = {"from": src, "to": dst}
        rs = requests.post(url, json=rq)
        self.process_response(rs)
        result = rs.json()
        print(result)
        return Game.parse_board(result["response"]), result["messages"]


