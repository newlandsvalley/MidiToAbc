#!/bin/sh
#############################################
#
# convert the lillasystern sample
#
# 
#
#############################################

../src/miditoabc -l "(0 % 8)" -d "(1 % 8)" -t "(9,8)" -r Polska -k Dn -m Major -n "lillasystern"  -i "../midi/lillasystern.mid" -o "../abc/lillasystern.abc"

 


