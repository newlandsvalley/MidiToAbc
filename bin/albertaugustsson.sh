#!/bin/sh
#############################################
#
# convert the Albert Augustsson engelska sample
#
# 
#
#############################################

../src/MidiToAbc -l "(1 % 8)" -d "(1 % 16)" -t "(2,4)" -r Engelska -k An -m Major -i "../midi/albertaugustssonengelska.mid" -o "../abc/albertaugustssonengelska.abc"

 


