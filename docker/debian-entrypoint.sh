#!/bin/bash
set -e

curl -sSL https://github.com/portnov/hcheckers/archive/master.zip -o master.zip
unzip master.zip && rm master.zip
cd hcheckers-master/
stack clean --full
bash prepare_debian_package.sh

cp ../*.deb /dst/
