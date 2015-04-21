#!/bin/sh
#############################################
#
# convert the short first polska sample
#
# 
#
#############################################

../src/MidiToAbc -l "(1 % 8)" -d "(1 % 16)" -t "(3,4)" -r Polska -k Dn -m Major -i "../midi/shortfirstpolska.mid" -o "../abc/shortfirstpolska.abc"

 


