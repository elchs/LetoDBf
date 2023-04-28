#!/bin/bash
# batch for GCC

### need to adapt
HB_DIR=/usr
HB_LIB=$HB_DIR/lib/harbour
HB_INC=$HB_DIR/include/harbour

### in harbour/addon/letodb directory
#OS_TYPE=linux
#HB_DIR=../..
#HB_LIB=$HRB_DIR/lib/$OS_TYPE/gcc
#HB_INC=$HRB_DIR/include/harbour

### no adapt below needed

LETO_DIR=../..
LETO_INC=$LETO_DIR/include
LETO_LIB=$LETO_DIR/lib

HARBOUR_LIBS="-lhbvm -lhbrtl -lhbcommon -lhbmacro -lgttrm -lhbrdd -lhbusrrdd -lrddntx -lrddcdx -lrddnsx -lrddfpt -lhbrdd -lhbhsx -lhbsix -lhbmacro -lhbcommon"
SYSTEM_LIBS="-lm -ldl -lrt"

### static build
gcc  $1.c -O3 -I$LETO_INC -I$HB_INC -L$LETO_LIB -L$HB_LIB -lleto -Wl,--start-group $HARBOUR_LIBS $SYSTEM_LIBS -Wl,--end-group -o $1

### dynamic build
#gcc  $1.c -O3 -I$LETO_INC -I$HB_INC -L$LETO_LIB -L$HB_LIB -lleto -lharbour -lhbzlib -o $1

