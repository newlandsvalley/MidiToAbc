#!/bin/sh
#############################################
#
# convert the ante falk polska sample
#
# 
#
#############################################

../src/MidiToAbc -l "(0 % 16)" -d "(1 % 16)" -t "(3,4)" -r Polska -k Dn -m Major -i "../midi/antefalkpolska.mid" -o "../abc/antefalkpolska.abc"


