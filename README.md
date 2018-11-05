# hcheckers README

## What this is

HCheckers is a relatively simple implementation of checkers board game (also known as "draughts").
The core is written in Haskell, and the GUI is written in Python + Qt5. Some
day, probably, there will be a web-based JS client to play from browser.

It is possible to play:

* Human vs computer (either user or computer plays white);
* Human vs human.

## What this is not

HCheckers is not about to compete with well-known and highly optimized
commercial checkers software. It will hardly do any good in playing versus
human checkers grossmaster.
HCheckers does not contain any pre-populated openings or endgames database, and
it actually does not know how to play checkers - it only knows the rules.

## Project goals

* Fun of development. For not-so-seasoned Haskell programmers, or people who are
  not-so-expirienced in writing games, this can show some examples. For that,
  I'm not going to optimize every possible bit: code readability is in
  priority. For that, HCheckers is not going to be faster than software written
  in C++ with economy of every bit of memory.
* Fun of game. HCheckers can play well enough for not-so-seasoned draughtsmen.

## Current state

At the moment, HCheckers has most of core functionality implemented, but it
still lacks a proper configuration subsystem and several other features. I hope
to implement them soon.

## (Planned) features

The code is general enough to implement a wide range of checkers variants:

* Russian
* Simple russian (russian draughts without kings)
* Diagonal russian (russian draughts with different initial setup)
* Spancirety (russian draughts on 8x10 board)
* English
* International draughts (10x10)
* Canadian draughts (12x12)
* ...

It is possible to implement different AI algorithms; currently there is only
one, based on standard alpha-beta pruning. The algorithm has some number of
parameters, which can be tuned to choose between better play and performance.

HCheckers can use persistent cache; it can help in calculating more turns, but
it can grow very large and eat a lot of RAM.

## Installation

```
$ sudo apt-get install stack
$ git clone https://github.com/portnov/hcheckers.git
$ cd hcheckers/
$ stack install
```

Run server:

```
$ stack exec hcheckers-exe
```

Run client:
```
$ cd python/
$ python hcheckers.py
```

