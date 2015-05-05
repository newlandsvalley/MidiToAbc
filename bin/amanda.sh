#!/bin/sh
#############################################
#
# convert the amanda sample
#
# 
#
#############################################

../src/miditoabc -t "0" -l "(1 % 8)" -d "(1 % 16)"  -r Polska -k Gn -m Minor -n "Amanda" -i "../midi/amanda.mid" -o "../abc/amanda.abc"

 


