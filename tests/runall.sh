#!/bin/bash

if [ "$1." = "." ]; then
   ADDR="127.0.0.1:2812"
else
   ADDR="$1"
fi

./ron $ADDR
./test_dbf $ADDR
./test_dbfe $ADDR
./test_file $ADDR
./test_filt $ADDR
./test_ta $ADDR
./test_tr $ADDR
./test_var $ADDR
./test_var $ADDR

