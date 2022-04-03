#!/usr/bin/python3

import sys
import os
from os.path import join, exists, dirname, abspath
import gettext
import logging
import argparse

bindir = abspath(dirname(sys.argv[0]))
if exists(join(bindir, "themes")):
    print("Development mode")
    sys.path.insert(0, dirname(bindir))

import hcheckers

def locate_share_dir():
    home = os.environ["HOME"]
    moduledir = abspath(dirname(hcheckers.__file__))
    bases = ["/usr/share/hcheckers", "/usr/local/share/hcheckers",
             join(home, ".local", "share", "hcheckers"),
             moduledir, bindir]
    for base in bases:
        themes = join(base, "themes")
        if exists(base) and exists(themes):
            return base
    raise Exception("Cannot locate share directory; checked were: {}".format(bases))

def locate_locales():
    share = locate_share_dir()
    locales = join(share, "locale")
    logging.info("Using locales at: {}".format(locales))
    return locales

if sys.version_info[0] == 2:
    gettext.install("hcheckers", localedir=locate_locales(), unicode=True)
else:
    gettext.install("hcheckers", localedir=locate_locales())

from hcheckers.common import *
from hcheckers.game import Game, AI

def print_board(size, board):
    rows, cols = size
    b = [[" " for i in range(cols)] for j in range(rows)]
    for loc in board:
        i,j = loc.row, loc.col
        b[i][j] = board[loc].print_notation()
    for row in reversed(b):
        print("".join(row))

def show_move(notation_by_label, move):
    first = notation_by_label[move.from_field]
    last = notation_by_label[move.steps[-1].field]
    is_capture = any(step.capture == True for step in move.steps)
    if is_capture:
        return "{}x{}".format(first, last)
    else:
        return "{}-{}".format(first, last)

def print_history(notation_by_label, history):
    for record in reversed(history):
        side = record["side"]
        move = Move.fromJson(record["move"])
        move_str = show_move(notation_by_label, move)
        print(f"{side}: {move_str}")

SERVER_URL = "http://localhost:8864"

parser = argparse.ArgumentParser(description = "Run computer-vs-computer game at HCheckers server")
parser.add_argument('-1', '--first', metavar="AI.JSON", required=True, help="AI settings for the first player")
parser.add_argument('-2', '--second', metavar="AI.JSON", required=True, help="AI settings for the first player")
parser.add_argument('-s', '--server', metavar="HTTP://SERVER:PORT", default=SERVER_URL, help="HCheckersd server URL")
parser.add_argument('-r', '--rules', metavar="RULES", default='russian', help="Checkers rules to be used")
parser.add_argument('-i', '--input', metavar="FILE.PDN|FEN", help="Initial position description")
parser.add_argument('-o', '--output', metavar="FILE.PDN", default="game.pdn", help="Output PDN file")

args = parser.parse_args()

game = Game(url = args.server)

ai1 = AI.from_json_file(args.first)
ai2 = AI.from_json_file(args.second)

kwargs = {}
if args.input:
    if args.input.upper().endswith(".PDN"):
        kwargs['pdn_path'] = args.input
    elif args.input.upper().endswith(".FEN"):
        kwargs['fen_path'] = args.input
    else:
        print("Unsupported initial position file format")
        sys.exit(1)

game_id, first_side = game.new_game(rules=args.rules, **kwargs)
game.attach_ai(FIRST, ai1)
game.attach_ai(SECOND, ai2)
game.run_game()
game.run_game_loop()

size, invert, notation, border_notation = game.get_notation(game.rules)
notation_by_label = dict()
for label, label_notation in notation:
    notation_by_label[Label.fromJson(label)] = label_notation

prev_len = 1
while True:
    rs = game.get_state()
    board = Game.parse_board(rs['board'])
    history = game.get_history()
    new_len = len(history)
    if new_len > prev_len:
        print_board(size, board)
        print("----")
        prev_len = new_len
    else:
        pass
    #print_history(notation_by_label, history)
    status = rs['status']
    if status != "Running":
        print(status)
        pdn = game.get_pdn()
        with open(args.output, 'w') as f:
            f.write(pdn)
        break

