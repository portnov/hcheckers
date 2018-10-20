# hcheckers README

## What this is

HCheckers is a relatively simple implementation of checkers board game (also known as "draughts").
The core is written in Haskell, and the GUI is written in Python + Qt5. Some
day, probably, there will be a web-based JS client to play from browser.

## What this is not

HCheckers is not about to compete with well-known and highly optimized commercial checkers
software. It will hardly do any good in playing versus human checkers
grossmaster.
HCheckers does not contain any pre-populated openings or endgames database, and
it actually does not know how to play checkers - it only knows the rules.

## Project goals

* Demonstrative. For not-so-seasoned Haskell programmers, or people who are not-so-expirienced in writing games,
  this can show some examples. For that, I'm not going to optimize every possible
  bit: code readability is in priority. For that, HCheckers is not going to be
  faster than software written in C++ with economy of every bit of memory.
* Fun. HCheckers can play well enough for not-so-seasoned draughtsmen.

## (Planned) features

For now, only Russian draughts rules are implemented. However, the code is general enough to implement a wide range of checkers variants:

* Russian (done)
* English
* International checkers (10x10)
* and so on

It is possible to implement different AI algorithms
