HCheckers server docker container
=================================

Plain docker container
----------------------

It is possible to build a docker container with hcheckers server, without
having to build any packages. This may be useful for systems for which there is
no (yet) package available.

1. Build a builder image (it will contain all necessary environment to build
   HCheckers from source) by running `build-plain-builder.sh` script.
2. Build `hcheckersd` image by running `build-plain.sh` script.
3. Now you can run HCheckers server with `run-plain.sh` script.

Building Ubuntu package
-----------------------

To build a package for Ubuntu:

1. Build a builder image by running `build-ubuntu-package.sh`.
2. `deb` file will be available under `target/` subdirectory.

