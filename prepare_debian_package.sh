#!/bin/bash
set -e

TARGET=hcheckersd_0.1.0.0.orig.tar.xz 
tar -v -cJ --exclude-vcs --exclude='*.log' --exclude='*.build' --exclude=./build --exclude=./.stack-work --exclude=./debian --exclude=./docker --exclude=./csv --exclude=./games --exclude=./python --exclude=./test --exclude='*.tar.xz' -f ../$TARGET .

LOCAL_INSTALL_ROOT=$(stack path --local-install-root)
LOCAL_INSTALL_ROOT=$(realpath --relative-to=$PWD $LOCAL_INSTALL_ROOT)
echo "$LOCAL_INSTALL_ROOT/bin/hcheckersd usr/bin" > debian/hcheckersd.install

dpkg-buildpackage -uc -us

