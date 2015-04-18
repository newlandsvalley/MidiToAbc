#!/bin/sh
#############################################
#
# convert the quadruplet polska sample
#
# 
#
#############################################

../src/MidiToAbc -l "(0 % 8)" -d "(1 % 8)" -t "(9,8)" -r Polska -k Gn -m Major -i "../midi/quadrupletpolska.midi" -o "../abc/quadrupletpolska.abc"

 


