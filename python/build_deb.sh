#!/bin/bash

python3 setup.py --command-packages=stdeb.command sdist_dsc bdist_deb
