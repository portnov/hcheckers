#!/bin/bash

HERE=$(dirname $0)

cd $HERE/../locale
for D in $(ls -d *)
do if [ -d $D ]
   then pushd $D/LC_MESSAGES/
        echo Building $D
        msgfmt -v -o hcheckers.mo hcheckers.po
        popd
   fi
done
