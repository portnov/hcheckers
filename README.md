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

But this is not either a "look-what-i-did-in-one-evening" project. HCheckers
has full support of several rules, has decent UI, supports UI localization, and
plays not so bad for amateur.

## Project goals

* Fun of development. For not-so-seasoned Haskell programmers, or people who are
  not-so-expirienced in writing games, this can show some examples. For that,
  I'm not going to optimize every possible bit: code readability is in
  priority. For that, HCheckers is not going to be faster than software written
  in C++ with economy of every bit of memory.
* Fun of game. HCheckers can play well enough for not-so-seasoned draughtsmen.


## Features

The code is general enough to implement a wide range of checkers variants.
The following are implemented at the moment:

* Russian
* Simple russian (russian draughts without kings)
* Diagonal russian (russian draughts with different initial setup)
* Spancirety (russian draughts on 8x10 board)
* English (checkers)
* International draughts (10x10)
* Brazilian (rules of international draughts on 8x8 board)
* Canadian draughts (12x12)
* Turkish draughts (orthogonal)
* Armenian draughts (Tama)

It is possible to implement different AI algorithms; currently there is only
one, based on standard alpha-beta pruning. The algorithm has some number of
parameters, which can be tuned to choose between better play and performance.

HCheckers can use persistent cache; it can help in calculating more turns, but
it can grow very large and eat a lot of RAM.

## Current state

At the moment, HCheckers has most of core functionality implemented, but there
are some outstanding issues (please refer to github's issue tracker).
Most wanted planned things to do are:

* User documentation (#22)
* Code documentation (#1)
* Spectators support (#9)

## Installation

### Server part

For the server part, there are two options available.

### Ubuntu package

Last package that I released is available in github releases. Please refer to
https://github.com/portnov/hcheckers/releases .

To build a newer package,

```
$ git clone https://github.com/portnov/hcheckers.git
$ cd hcheckers/docker/
$ ./build-ubuntu-package.sh
```

The package will be available under `docker/target` subdirectory. 
Use `sudo dpkg -i hcheckersd_0.1.0.0-1_amd64.deb` to install it.

### Docker image

For non-debian based systems, the only "easily distributed" form for now is the
docker container.

```
$ git clone https://github.com/portnov/hcheckers.git
$ cd hcheckers/docker/
$ ./build-plain-builder.sh
$ ./build-plain.sh
$ ./run-plain.sh
```

### Client part

Python client can be installed in two ways:

1) Via `pip`:

```
$ cd hcheckers/python/
$ sudo pip3 install .
```

2) Using debian package (on debian-based systems).

Last package that I released is available in github releases. Please refer to
https://github.com/portnov/hcheckers/releases .

To build a newer package, execute

```
$ sudo apt-get install python3-stdeb
$ cd hcheckers/python/
$ ./build_deb.sh
```

To install a package, do

```
$ sudo apt install python3-pyqt5 python3-pyqt5.qtsvg python3-pyqt5.qtmultimedia
$ sudo dpkg -i deb_dist/python3-hcheckers_0.1.0.0-1_all.deb
```

After client is installed (either via `pip` or `deb` package), you can run it with

```
$ hcheckersc.py
```

## Running development version

You can run HCheckers without actually installing it; it is mostly useful while developing it.

```
$ sudo apt-get install stack
$ cd hcheckers/
$ stack build
```

Run server:

```
$ stack exec hcheckersd
```

Run client:
```
$ cd python/
$ python hcheckersc.py
```

