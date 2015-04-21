#!/bin/sh
#############################################
#
# convert the amanda sample
#
# 
#
#############################################

../src/MidiToAbc -l "(1 % 8)" -d "(1 % 16)" -t "(3,4)" -r Polska -k Gn -m Minor -i "../midi/amanda.mid" -o "../abc/amanda.abc"

 


