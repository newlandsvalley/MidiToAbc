#!/bin/sh
#############################################
#
# convert the quadruplet polska sample
#
# 
#
#############################################

../src/miditoabc  -t "0" -l "(0 % 8)" -d "(1 % 8)" -r Polska -k Gn -m Major -n "quadruplet"  -i "../midi/quadrupletpolska.mid" -o "../abc/quadrupletpolska.abc"

 


