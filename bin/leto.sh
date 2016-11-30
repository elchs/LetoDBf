#!/bin/bash

#CMD=`letodb`
#./letodb stop
rm letodbf.log

ss -tan state time-wait > ss.log
while grep -q 281 ss.log
do
    sleep 3s
    ss -tan state time-wait > ss.log
done

rm ss.log
./letodb

reset
beep -f 2500 -l 1000
echo "LetoDB runs"
