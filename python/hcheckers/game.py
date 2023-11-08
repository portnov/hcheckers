
# REST client

from os.path import join, basename
import requests
import logging
from threading import Thread, Lock
import json
import traceback

from hcheckers.common import *

class MoveRequest(Thread):
    def __init__(self, owner, url, proxies, rq):
        Thread.__init__(self)
        self.owner = owner
        self.url = url
        self.proxies = proxies
        self.rq = rq

    def run(self):
        try:
            self.owner.move_lock.acquire()
            self.owner.move_exception = None
            rs = Game.post_static(self.url, json=self.rq, **self.proxies)
            result = rs.json()
            #logging.debug(result)
            messages = result["messages"]
            board = result["response"]["board"]
            session = result["response"]["poll"]
            self.owner.last_move_result = Game.parse_board(board), session, messages
        except Exception as e:
            self.owner.move_exception = e
        finally:
            self.owner.move_lock.release()

def get_title_from_json(json_data):
    titles = json_data["title"]
    lang = get_current_language()
    if lang not in titles:
        lang = "en"
    if lang in titles:
        return titles[lang]
    else:
        return json_data["slug"]

class AI(object):
    def __init__(self, **kwargs):
        self.title = "Default AI"
        self.notes = ""
        self.from_server = False
        self.slug = None
        self.depth = 2
        self.max_combination_depth = 6
        self.dynamic_depth = 6
        self.background_thinking = False
        self.deeper_if_bad = False
        self.deeper_if_ambigous = False
        self.depth_if_ambigous = 10
        self.moves_bound_low = 4
        self.moves_bound_high = 8
        self.start_depth = None
        self.use_positional_score = True
        self.timeout = None
        self.use_timeout = False
        self.random_opening_depth = 1
        self.random_opening_options = 1

        self.mobility_weight = 30
        self.backyard_weight = 14
        self.center_weight = 16
        self.border_weight = -16
        self.opposite_side_weight = 32
        self.backed_weight = 24
        self.asymetry_weight = -12
        self.temp_asymetry_weight = -12
        self.pre_king_weight = 28
        self.king_coef = 3
        self.positional_king_weight = 0
        self.attacked_man_coef = -40
        self.attacked_king_coef = -80
        self.king_on_key_field_weight = 10
        self.man_blocked_by_king_weight = -20

        self.accept_draw = ALWAYS_ACCEPT

        self.extra = None

        for key in kwargs:
            setattr(self, key, kwargs[key])
    
    def copy(self):
        json_data = self.params()
        ai = AI()
        ai.from_server = self.from_server
        ai.slug = self.slug
        ai.load_json(json_data)
        ai.title = self.title
        return ai

    @classmethod
    def from_json(cls, name, json_data):
        ai = AI()
        ai.title = name
        ai.load_json(json_data)
        return ai

    @classmethod
    def from_json_file(cls, path):
        with open(path) as f:
            text = f.read()
            json_data = json.loads(text)
            ai = AI()
            ai.title = basename(path)
            ai.load_json(json_data)
            return ai

    @classmethod
    def from_settings(cls, settings):
        ai = AI()
        ai.from_server = False
        ai.title = settings.value("title")
        ai.notes = settings.value("notes")
        ai.depth = settings.value("depth", type=int)
        ai.max_combination_depth = settings.value("max_combination_depth", type=int)
        ai.dynamic_depth = settings.value("dynamic_depth", type=int)
        ai.background_thinking = settings.value("background_thinking", type=bool)
        ai.start_depth = settings.value("start_depth", type=int)
        ai.deeper_if_bad = settings.value("deeper_if_bad", type=bool)
        ai.deeper_if_ambigous = settings.value("deeper_if_ambigous", type=bool)
        ai.depth_if_ambigous = settings.value("depth_if_ambigous", type=int)
        ai.moves_bound_low = settings.value("moves_bound_low", type=int)
        ai.moves_bound_high = settings.value("moves_bound_high", type=int)
        ai.use_positional_score = settings.value("use_positional_score", type=bool)
        ai.use_timeout = settings.value("use_timeout", type=bool)
        ai.timeout = settings.value("timeout", type=int)
        ai.random_opening_depth = settings.value("random_opening_depth", ai.random_opening_depth, type=int)
        ai.random_opening_options = settings.value("random_opening_options", ai.random_opening_options, type=int)

        ai.mobility_weight = settings.value("mobility_weight", ai.mobility_weight, type=int)
        ai.backyard_weight = settings.value("backyard_weight", ai.backyard_weight, type=int)
        ai.center_weight = settings.value("center_weight", ai.center_weight, type=int)
        ai.border_weight = settings.value("border_weight", ai.border_weight, type=int)
        ai.opposite_side_weight = settings.value("opposite_side_weight", ai.opposite_side_weight, type=int)
        ai.backed_weight = settings.value("backed_weight", ai.backed_weight, type=int)
        ai.asymetry_weight = settings.value("asymetry_weight", ai.asymetry_weight, type=int)
        ai.temp_asymetry_weight = settings.value("temp_asymetry_weight", ai.temp_asymetry_weight, type=int)
        ai.pre_king_weight = settings.value("pre_king_weight", ai.pre_king_weight, type=int)
        ai.king_coef = settings.value("king_coef", ai.king_coef, type=int)
        ai.positional_king_weight = settings.value("positional_king_weight", ai.positional_king_weight, type=int)
        ai.attacked_man_coef = settings.value("attacked_man_coef", ai.attacked_man_coef, type=int)
        ai.attacked_king_coef = settings.value("attacked_king_coef", ai.attacked_king_coef, type=int)
        ai.king_on_key_field_weight = settings.value("king_on_key_field_weight", ai.king_on_key_field_weight, type=int)
        ai.man_blocked_by_king_weight = settings.value("man_blocked_by_king_weight", ai.man_blocked_by_king_weight, type=int)

        ai.accept_draw = settings.value("accept_draw", ai.accept_draw)

        ai.extra = settings.value("extra", type=str)
        return ai
    
    @classmethod
    def list_from_settings(cls, share_dir, settings):
        result = []
        size = settings.beginReadArray("AI")
        for idx in range(size):
            settings.setArrayIndex(idx)
            ai = AI.from_settings(settings)
            result.append(ai)
        settings.endArray()
        return result

    def to_settings(self, settings):
        settings.setValue("title", self.title)
        settings.setValue("notes", self.notes)
        settings.setValue("depth", self.depth)
        settings.setValue("max_combination_depth", self.max_combination_depth)
        settings.setValue("dynamic_depth", self.dynamic_depth)
        settings.setValue("background_thinking", self.background_thinking)
        settings.setValue("start_depth", self.start_depth)
        settings.setValue("deeper_if_bad", self.deeper_if_bad)
        settings.setValue("deeper_if_ambigous", self.deeper_if_ambigous)
        settings.setValue("depth_if_ambigous", self.depth_if_ambigous)
        settings.setValue("moves_bound_low", self.moves_bound_low)
        settings.setValue("moves_bound_high", self.moves_bound_high)
        settings.setValue("use_positional_score", self.use_positional_score)
        settings.setValue("use_timeout", self.use_timeout)
        settings.setValue("timeout", self.timeout)
        settings.setValue("random_opening_depth", self.random_opening_depth)
        settings.setValue("random_opening_options", self.random_opening_options)

        settings.setValue("mobility_weight", self.mobility_weight)
        settings.setValue("backyard_weight", self.backyard_weight)
        settings.setValue("center_weight", self.center_weight)
        settings.setValue("border_weight", self.border_weight)
        settings.setValue("opposite_side_weight", self.opposite_side_weight)
        settings.setValue("backed_weight", self.backed_weight)
        settings.setValue("asymetry_weight", self.asymetry_weight)
        settings.setValue("temp_asymetry_weight", self.temp_asymetry_weight)
        settings.setValue("pre_king_weight", self.pre_king_weight)
        settings.setValue("king_coef", self.king_coef)
        settings.setValue("positional_king_weight", self.positional_king_weight)
        settings.setValue("attacked_man_coef", self.attacked_man_coef)
        settings.setValue("attacked_king_coef", self.attacked_king_coef)
        settings.setValue("king_on_key_field_weight", self.king_on_key_field_weight)
        settings.setValue("man_blocked_by_king_weight", self.man_blocked_by_king_weight)

        settings.setValue("accept_draw", self.accept_draw)

        if self.extra is not None:
            settings.setValue("extra", self.extra.strip())

    def params(self):
        d = {
            "depth": self.depth,
            "start_depth": self.start_depth,
            "max_combination_depth": self.max_combination_depth,
            "background_thinking": self.background_thinking,
            "dynamic_depth": self.dynamic_depth,
            "deeper_if_bad": self.deeper_if_bad,
            "depth_if_ambigous": self.depth_if_ambigous if self.deeper_if_ambigous else None,
            "moves_bound_low": self.moves_bound_low,
            "moves_bound_high": self.moves_bound_high,
            "use_positional_score": self.use_positional_score,
            "time": self.timeout if self.use_timeout else None,
            "random_opening_depth": self.random_opening_depth,
            "random_opening_options": self.random_opening_options,

            "mobility_weight": self.mobility_weight,
            "backyard_weight": self.backyard_weight,
            "center_weight": self.center_weight,
            "border_weight": self.border_weight,
            "opposite_side_weight": self.opposite_side_weight,
            "backed_weight": self.backed_weight,
            "asymetry_weight": self.asymetry_weight,
            "temp_asymetry_weight": self.temp_asymetry_weight,
            "king_coef": self.king_coef,
            "positional_king_weight": self.positional_king_weight,
            "pre_king_weight": self.pre_king_weight,
            "attacked_man_coef": self.attacked_man_coef,
            "attacked_king_coef": self.attacked_king_coef,
            "king_on_key_field_weight": self.king_on_key_field_weight,
            "man_blocked_by_king_weight": self.man_blocked_by_king_weight,

            "accept_draw": self.accept_draw
        }
        if self.extra:
            try:
                extra = json.loads(self.extra)
                d.update(extra)
            except Exception as e:
                logging.error("Can't parse:")
                logging.error(self.extra)
                logging.exception(e)
        return d
    
    def load_json(self, settings):
        extra = dict()
        for key in settings:
            if hasattr(self, key):
                setattr(self, key, settings[key])
            else:
                extra[key] = settings[key]

        self.deeper_if_ambigous = (self.depth_if_ambigous is not None)

        if extra:
            self.extra = json.dumps(extra)
        else:
            self.extra = ""

class GameSettings(object):
    def __init__(self):
        self.user_name = None
        self.ai = None
        self.rules = None
        self.timing = None
        self.run_now = False
        self.url = None
        self.user_turn_first = True
        self.board_setup = False
        self.action = None
        self.board_type = None
        self.fen_path = None
        self.pdn_path = None
        self.previous_board_game = None
        self.use_random_board_preset = False

class Game(object):
    def __init__(self, url=None, proxy_usage=PROXY_SYSTEM, proxy_address=None):
        if url is None:
            url = DEFAULT_SERVER_URL
        self.base_url = url
        self.proxy_usage = proxy_usage
        self.proxy_address = proxy_address
        self.game_id = None
        self.user_name = None
        self.user_side = None
        self.ai_side = None
        self.last_move_result = None
        self.rules = None
        self.timing = None
        self.move_thread = None
        self.move_lock = Lock()
        self.move_exception = None
        self.finished = False
        self.draw_state = None
        self._colors = None
        self.undo_count = 0
        self.hint_count = 0

    def is_active(self):
        defined = not (self.base_url is None or self.user_name is None or self.game_id is None)
        return defined and not self.finished

    def is_connected(self):
        return not (self.base_url is None or self.user_name is None or self.game_id is None)

    @staticmethod
    def process_response(rs, action=None):
        if rs is None:
            return
        if rs.status_code != requests.codes.ok:
            raise RequestError(rs)

    @staticmethod
    def get_proxies_static(proxy_usage, proxy_address):
        if proxy_usage == PROXY_SYSTEM:
            kwargs = {}
        elif proxy_usage == PROXY_CUSTOM:
            kwargs = dict(proxies = {
                        'http': proxy_address,
                        'https': proxy_address
                })
        else:
            kwargs = dict(proxies={'http': None, 'https': None})
        return kwargs

    def get_proxies(self):
        return Game.get_proxies_static(self.proxy_usage, self.proxy_address)
    
    @staticmethod
    def get_static(url, *args, **kwargs):
        rs = requests.get(url, *args, **kwargs)
        Game.process_response(rs)
        return rs

    def get(self, url, *args, **kwargs):
        proxies = self.get_proxies()
        kwargs.update(proxies)
        return Game.get_static(url, *args, **kwargs)

    @staticmethod
    def post_static(url, *args, **kwargs):
        rs = requests.post(url, *args, **kwargs)
        Game.process_response(rs)
        return rs

    def post(self, url, *args, **kwargs):
        proxies = self.get_proxies()
        kwargs.update(proxies)
        return Game.post_static(url, *args, **kwargs)

    @staticmethod
    def check_server(server_url, proxies):
        try:
            url = join(server_url, "status")
            rs = Game.get_static(url, proxies=proxies)
            status = rs.json()["status"]
            return status == "ready"
        except Exception as e:
            print(e)
            #traceback.print_exc()
            return False

    def is_custom_ai_settings_enabled(self):
        url = join(self.base_url, "ai", "custom", "permissions")
        try:
            rs = self.get(url)
            response = rs.json()
            return response["custom_settings_enabled"]
        except requests.exceptions.ConnectionError as e:
            return True
        except RequestError:
            return True

    def get_ai_settings(self):
        url = join(self.base_url, "ai", "default")
        try:
            rs = self.get(url)
            result = []
            for item in rs.json():
                ai = AI.from_json(get_title_from_json(item), item["settings"])
                ai.from_server = True
                ai.slug = item['slug']
                result.append(ai)
            return result
        except requests.exceptions.ConnectionError as e:
            return []
        except RequestError:
            return []

    def get_timing_options(self, rules=None):
        url = join(self.base_url, "timing")
        try:
            if rules is None:
                params = None
            else:
                params = {"rules": rules}
            rs = self.get(url, params=params)
            result = []
            for slug, cfg in rs.json().items():
                title = get_title_from_json(cfg)
                result.append((slug, title))
            return result
        except requests.exceptions.ConnectionError as e:
            return []
        except RequestError:
            return []

    def get_games(self, rules=None):
        if rules is not None:
            url = join(self.base_url, "lobby", rules)
        else:
            url = join(self.base_url, "lobby")
        rs = self.get(url)
        result = rs.json()["response"]
        return result

    def new_game(self, rules, timing=None, board=None, fen_path=None, pdn_path=None, previous_board_game = None, use_random_board_preset = False):
        url = join(self.base_url, "game", "new")
        rq = {"rules": rules}
        if board is not None:
            rq["board"] = board
        elif use_random_board_preset:
            rq["use_random_board_preset"] = True
        elif previous_board_game is not None:
            rq["previous_board"] = previous_board_game
        elif fen_path is not None:
            fen_text = open(fen_path).read()
            rq["fen"] = fen_text
        elif pdn_path is not None:
            pdn_text = open(pdn_path).read()
            rq["pdn"] = pdn_text
        self.timing = timing
        rq["timing"] = self.timing
        rs = self.post(url, json=rq)
        result = rs.json()
        self.game_id = result["response"]["id"]
        self.draw_state = None
        first_side = result["response"]["turn"]
        first_move_first = first_side == 'First'
        first_are_black = self.get_invert_colors(rules)
        if first_move_first != first_are_black:
            first_color = _("White")
        else:
            first_color = _("Black")

        logging.info(_("Newly created game ID: {}. First side to move: {}.").format(self.game_id, first_color))
        self.rules = rules
        self.finished = False
        self._colors = None
        return self.game_id, first_side

    def get_version(self):
        url = join(self.base_url, "version")
        try:
            rs = self.get(url)
            return rs.json()
        except requests.exceptions.ConnectionError as e:
            return dict(release="not present", hash="no connection")
        except RequestError as e:
            if e.rs.status_code == requests.codes.not_found:
                return dict(release="unknown", hash="old")
            else:
                raise e

    def get_notation(self, rules):
        url = join(self.base_url, "notation", rules)
        rs = self.get(url)
        result = rs.json()["response"]
        invert = result["orientation"] == 'SecondAtBottom'
        return result["size"], invert, result["notation"], result["border_notation"]

    def get_topology(self, rules):
        url = join(self.base_url, "topology", rules)
        rs = self.get(url)
        result = rs.json()["response"]
        return result["topology"]
    
    def get_invert_colors(self, rules):
        _size, invert, _notation, _border = self.get_notation(rules)
        return invert

    def get_color_mapping(self, rules):
        """
        Return a tuple (side1, side2), where
        side1 is one who uses white pieces, and
        side2 is one who uses black pieces.
        """
        _size, invert, _notation, _border = self.get_notation(rules)
        if invert:
            return SECOND, FIRST
        else:
            return FIRST, SECOND

    def get_colors(self):
        if self._colors is None:
            invert = self.get_invert_colors(self.rules)
            if invert:
                self._colors = _("Black"), _("White")
            else:
                self._colors = _("White"), _("Black")
        return self._colors

    def register_user(self, name, side):
        self.user_name = name
        self.user_side = side
        url = join(self.base_url, "game", self.game_id, "attach", name, str(side))
        rs = self.post(url)
        result = rs.json()
        return result

    def attach_ai(self, side, ai):
        self.ai_side = side
        url = join(self.base_url, "game", self.game_id, "attach", "ai", str(side))
        if ai.from_server:
            logging.info(_("AI settings on server: {}").format(ai.slug))
            rq = {"ai": "default", "name": ai.slug, "from_server": True}
        else:
            ai_params = ai.params()
            logging.info(_("AI parameters:\n{}").format(json.dumps(ai_params, indent=2)))
            rq = {"ai": "default", "name": ai.title, "params": ai_params}
        try:
            rs = self.post(url, json=rq)
            result = rs.json()
            return result
        except RequestError as e:
            self.game_id = None
            raise e
    
    def spectate(self, user_name):
        url = join(self.base_url, "game", self.game_id, "attach", user_name, "spectate")
        rs = self.post(url)
        result = rs.json()
        return result

    def run_game(self):
        url = join(self.base_url, "game", self.game_id, "run")
        rs = self.post(url)
        result = rs.json()

    def run_game_loop(self):
        url = join(self.base_url, "game", self.game_id, "run", "loop")
        rs = self.post(url)
        result = rs.json()
        return result

    def start_new_game(self, user_name, rules="russian", timing=None, board=None, fen_path=None, pdn_path=None, previous_board_game = None, use_random_board_preset=False, user_turn_first=True, ai=None):
        if ai is None:
            ai = AI()
        game_id, first_side = self.new_game(rules, timing=timing, board=board, fen_path=fen_path, pdn_path=pdn_path, previous_board_game = previous_board_game, use_random_board_preset=use_random_board_preset)
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

    def get_move_with_result(self, turn_idx, side):
        url = join(self.base_url, "game", self.game_id, "history", str(turn_idx), str(side))
        rs = self.get(url)
        result = rs.json()["response"]
        return Move.fromJson(result["move"]), Game.parse_board(result["prev_board"]), Game.parse_board(result["next_board"])

    def get_initial_board(self):
        url = join(self.base_url, "game", self.game_id, "history", "init", "board")
        rs = self.get(url)
        result = rs.json()
        return Game.parse_board(result["response"])
    
    def poll(self, user_name=None):
        if user_name is None:
            user_name = self.user_name
        url = join(self.base_url, "poll", user_name)
        rs = self.get(url)
        result = rs.json()
        messages = result["response"]
        self._process_messages(messages)
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
    
    def ai_hint(self):
        url = join(self.base_url, "game", self.game_id, "ai", "hint", self.user_name)
        rs = self.get(url)
        result = rs.json()
        session = result["response"]["poll"]
        self.hint_count = result["response"]["hint_count"]
        messages = result["messages"]
        self._process_messages(messages)
        return session

    def undo(self):
        url = join(self.base_url, "game", self.game_id, "undo", self.user_name)
        rs = self.post(url)
        result = rs.json()
        response = result["response"]
        board = Game.parse_board(response["board"])
        self.undo_count = response["undo_count"]
        return board
    
    def request_draw(self):
        url = join(self.base_url, "game", self.game_id, "draw", "request", self.user_name)
        rs = self.post(url)
        result = rs.json()
        session = result["response"].get("poll", None)
        messages = result["messages"]
        self._process_messages(messages)
        logging.info(_("Offer for a draw posted."))
        self.draw_state = WE_REQUESTED_DRAW
        return session, messages
    
    def accept_draw(self, accept):
        if accept:
            verb = "accept"
        else:
            verb = "decline"
        url = join(self.base_url, "game", self.game_id, "draw", verb, self.user_name)
        rs = self.post(url)
        result = rs.json()
        messages = result["messages"]
        self._process_messages(messages)
        if accept:
            logging.info(_("You accepted draw."))
        else:
            logging.info(_("You declined draw."))
        self.draw_state = None
        return messages

    def capitulate(self):
        url = join(self.base_url, "game", self.game_id, "capitulate", self.user_name)
        rs = self.post(url)
        result = rs.json()
        messages = result["messages"]
        self._process_messages(messages)
        logging.info(_("Game #{}: you capitulated.").format(self.game_id))
        return messages
    
    def shutdown(self):
        url = join(self.base_url, "server", "shutdown")
        rs = self.post(url)
        logging.info(_("Server shutdown requested."))
    
    def begin_move(self, src, dst):
        if self.move_thread is not None:
            raise Exception("it seems get_move_result() was not called after previous begin_move()")
        url = join(self.base_url, "game", self.game_id, "move", self.user_name)
        rq = {"from": src.json(), "to": dst.json()}
        self.last_move_result = None
        self.move_thread = MoveRequest(self, url, self.get_proxies(), rq)
        self.move_thread.start()
        logging.debug("Thread was started")

    def get_move_result(self):
        if self.move_thread is None:
            raise Exception("get_move_result() must be called after begin_move()")
        try:
            self.move_lock.acquire()
            if self.move_exception is not None:
                raise self.move_exception
            result, session, messages = self.last_move_result
            self._process_messages(messages)
            self.last_move_result = None
            self.move_thread = None
            return result, session, messages
        finally:
            self.move_lock.release()

    def poll_move(self, session_id):
        url = join(self.base_url, "poll", "move", self.game_id, session_id)
        rs = self.get(url)
        result = rs.json()
        messages = result["messages"]
        response = result["response"]
        self._process_messages(messages)
        return response, messages

    def stop_ai(self, session_id):
        url = join(self.base_url, "game", self.game_id, "move", self.user_name, str(session_id), "stop")
        rs = self.post(url)
        result = rs.json()
        messages = result["messages"]
        self._process_messages(messages)
        logging.info(_("Requested AI to stop thinking"))

    def _process_messages(self, messages):
        for message in messages:
            self._process_message(message)

    def _process_message(self, message):
        if "result" in message:
            self.finished = True
        elif message.get("draw",None) == "requested":
            self.draw_state = DRAW_REQUESTED_FROM_US
        elif "draw_accepted"in message:
            self.draw_state = None

