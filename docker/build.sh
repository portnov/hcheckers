#!/bin/bash
set -e

mkdir -p build

docker run --name hcheckers-builder --rm -v $(pwd)/build:/dst hcheckers-builder

docker build -t hcheckersd .
