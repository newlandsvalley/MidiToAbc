#!/bin/sh
#############################################
#
# convert the brudestykke sample
#
# 
#
#############################################

../src/MidiToAbc -l "(0 % 16)" -d "(1 % 16)" -t "(2,4)" -r Marsch -k An -m Major -i "../midi/brudemarsch1.mid" -o "../abc/brudemarsch1.abc"

