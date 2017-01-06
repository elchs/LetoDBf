#!/bin/bash

rm letodbf.log

while ss -tan state time-wait | grep -q 281
do
    sleep 3s
done

while ss -tan state last-ack | grep -q 281
do
    sleep 3s
done

./letodb

# beep -f 2500 -l 1000
echo "LetoDB runs"
