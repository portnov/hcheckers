#!/bin/bash
set -e

SRC=$(realpath $PWD/..)

docker build -t hcheckers-debian -f Dockerfile.debian .

docker run --name hcheckers-debian --rm -v $(pwd)/target:/dst -v $(pwd)/stack:/root/.stack -v $SRC:/src hcheckers-debian


