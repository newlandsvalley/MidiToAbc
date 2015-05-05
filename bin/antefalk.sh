#!/bin/sh
#############################################
#
# convert the ante falk polska sample
#
# 
#
#############################################

../src/miditoabc -t "0" -l "(0 % 16)" -d "(1 % 16)"  -r Polska -k Dn -m Major -n "Ante Falk" -i "../midi/antefalkpolska.mid" -o "../abc/antefalkpolska.abc"


