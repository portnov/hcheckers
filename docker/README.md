HCheckers server docker container
=================================

This directory contains all necessary to build and run HCheckers server
instance in a docker container.

Usage:

1. Build a builder image (it will contain all necessary environment to build
   HCheckers from source) by running `build-builder.sh` script.
2. Build `hcheckersd` image by running `build.sh` script.
3. Now you can run HCheckers server with `run.sh` script.
