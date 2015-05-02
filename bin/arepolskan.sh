#!/bin/sh
#############################################
#
# convert the årepolskan sample
#
# 
#
#############################################

../src/miditoabc -l "(1 % 8)" -d "(1 % 8)" -t "(9,8)" -r Polska -k Gn -m Major -n "årepolskan" -i "../midi/årepolskan.mid" -o "../abc/årepolskan.abc"

 


