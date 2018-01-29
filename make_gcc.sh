#!/bin/bash

if ! [ -e lib ]; then
   mkdir lib
   chmod a+w+r+x lib
fi
if ! [ -e obj ]; then
   mkdir obj
   chmod a+w+r+x obj
fi
if ! [ -e obj/api ]; then
   mkdir obj/api
   chmod a+w+r+x obj/api
fi

make -fmakefile.gcc $1 $2 $3 > make_gcc.log
