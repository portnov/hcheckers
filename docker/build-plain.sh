#!/bin/bash
set -e

mkdir -p build target

docker run --name hcheckers-builder --rm -v $(pwd)/target:/dst -v $(pwd)/build:/src/hcheckers-master/build hcheckers-builder

docker build -t hcheckersd .
