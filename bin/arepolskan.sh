#!/bin/sh
#############################################
#
# convert the årepolskan sample
#
# 
#
#############################################

../src/miditoabc -t "0" -l "(1 % 8)" -d "(1 % 8)"  -r Polska -k Gn -m Major -n "årepolskan" -i "../midi/årepolskan.mid" -o "../abc/årepolskan.abc"

 


