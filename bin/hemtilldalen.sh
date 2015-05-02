#!/bin/sh
#############################################
#
# convert the hem till Dalen sample
#
# 
#
#############################################

../src/miditoabc -l "(1 % 4)" -d "(1 % 8)" -t "(3,4)" -r Waltz -k Gn -m Major -n "hem till dalen" -i "../midi/hemtilldalen.mid" -o "../abc/hemtilldalen.abc"



