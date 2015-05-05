#!/bin/sh
#############################################
#
# convert the Albert Augustsson engelska sample
#
# 
#
#############################################

../src/miditoabc -t "0" -l "(1 % 8)" -d "(1 % 16)"  -r Engelska -k An -m Major -n "Albert Augustsson" -i "../midi/albertaugustssonengelska.mid" -o "../abc/albertaugustssonengelska.abc"

 


