#!/bin/sh
#############################################
#
# convert the amanda sample
#
# 
#
#############################################

../src/miditoabc -l "(1 % 8)" -d "(1 % 16)" -t "(3,4)" -r Polska -k Gn -m Minor -n "Amanda" -i "../midi/amanda.mid" -o "../abc/amanda.abc"

 


