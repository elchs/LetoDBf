#!/bin/bash

# following '281' checks for socket ports: 281x
# aka for LetoDBf default two ports 2812 & 2813

while ss -tan state time-wait | grep -q 281
do
    sleep 3s
done

while ss -tan state last-ack | grep -q 281
do
    sleep 3s
done


if ss -tl | grep -q 281
then
    echo "LetoDB already started"
else
    if [ -f letodbf.log ]; then
       rm letodbf.log
    fi

    if [ $# -gt 0 ]; then
       ./letodb config $1
    else
       ./letodb
    fi
    echo "LetoDB runs"
fi

# beep -f 2500 -l 1000

