#!/bin/bash
set -e

SRC=$(realpath $PWD/..)

docker build -t hcheckers-ubuntu -f Dockerfile.ubuntu .

docker run --name hcheckers-ubuntu --rm -v $(pwd)/target:/dst -v $(pwd)/stack:/root/.stack -v $SRC:/src hcheckers-ubuntu

