#!/bin/bash
set -e
set -x

VERSION=2023.10
TARGET=hcheckersd_$VERSION.orig.tar.xz 
tar -v -cJ --exclude-vcs --exclude=./ai --exclude='*.json' --exclude='*.log*' --exclude='*.hp' --exclude='*.eventlog' --exclude='*.prof' --exclude='*.build' --exclude=./work --exclude=./.stack-work --exclude=./debian --exclude=./docker --exclude=./csv --exclude=./games --exclude=./python --exclude=./test --exclude=./genetics --exclude=./pdn_examples --exclude='*.tar.xz' -f ../$TARGET .

LOCAL_INSTALL_ROOT=$(stack path --local-install-root --allow-different-user)
LOCAL_INSTALL_ROOT=$(realpath --relative-to=$PWD $LOCAL_INSTALL_ROOT)
echo "$LOCAL_INSTALL_ROOT/bin/hcheckersd usr/bin" > debian/hcheckersd.install
echo "initial_positions usr/share/hcheckers" >> debian/hcheckersd.install
echo "ai_settings/* etc/hcheckers/ai" >> debian/hcheckersd.install
echo "timing.yaml etc/hcheckers" >> debian/hcheckersd.install

debuild -uc -us

