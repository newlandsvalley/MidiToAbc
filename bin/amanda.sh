#!/bin/sh
#############################################
#
# convert the amanda sample
#
# 
#
#############################################

../src/MidiToAbc -l "(1 % 8)" -d "(1 % 16)" -t "(9,8)" -r Polska -k Gn -m Minor -i "../midi/amanda.midi" -o "../abc/amanda.abc"

 


