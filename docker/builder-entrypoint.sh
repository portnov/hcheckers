#!/bin/bash
set -e

curl -sSL https://github.com/portnov/hcheckers/archive/master.zip -o master.zip
unzip master.zip && rm master.zip
cd hcheckers-master/
stack install --work-dir=./build --allow-different-user

cp /root/.local/bin/hcheckersd /dst/
