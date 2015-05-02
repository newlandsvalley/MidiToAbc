#!/bin/sh
#############################################
#
# convert the brudestykke sample
#
# 
#
#############################################

../src/miditoabc -l "(0 % 16)" -d "(1 % 16)" -t "(2,4)" -r Marsch -k An -m Major -n "brudestykke" -i "../midi/brudemarsch1.mid" -o "../abc/brudemarsch1.abc"

