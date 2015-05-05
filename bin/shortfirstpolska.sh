#!/bin/sh
#############################################
#
# convert the short first polska sample
#
# 
#
#############################################

../src/miditoabc -t "0" -l "(1 % 8)" -d "(1 % 16)"  -r Polska -k Dn -m Major -n "short first" -i "../midi/shortfirstpolska.mid" -o "../abc/shortfirstpolska.abc"

 


