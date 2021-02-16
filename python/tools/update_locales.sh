#!/bin/bash

HERE=$(dirname $0)
cd $HERE/../hcheckers/

pygettext3 -v -o locale/hcheckers.pot *.py
cd locale
for D in $(ls -d *)
do if [ -d $D ]
   then pushd $D/LC_MESSAGES
        msgmerge -v -U hcheckers.po ../../hcheckers.pot
        popd
   fi
done

