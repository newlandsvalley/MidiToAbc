#!/bin/sh
#############################################
#
# convert the hem till Dalen sample
#
# 
#
#############################################

../src/MidiToAbc -l "(1 % 4)" -d "(1 % 8)" -t "(3,4)" -r Waltz -k Gn -m Major -i "../midi/hemtilldalen.midi" -o "../abc/hemtilldalen.abc"



