#!/bin/sh
#############################################
#
# convert the lillasystern sample
#
# 
#
#############################################

../src/MidiToAbc -l "(0 % 8)" -d "(1 % 8)" -t "(9,8)" -r Polska -k Dn -m Major -i "../midi/lillasystern.midi" -o "../abc/lillasystern.abc"

 


