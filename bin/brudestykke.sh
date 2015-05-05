#!/bin/sh
#############################################
#
# convert the brudestykke sample
#
# 
#
#############################################

../src/miditoabc -t "0" -l "(0 % 16)" -d "(1 % 16)"  -r Marsch -k An -m Major -n "brudestykke" -i "../midi/brudemarsch1.mid" -o "../abc/brudemarsch1.abc"

