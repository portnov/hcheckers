#!/bin/bash

set -e

docker build -t hcheckers-builder -f Dockerfile.builder .
