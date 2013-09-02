#!/bin/bash
I=1
while true; do
  sleep 0.01;
  CPID=`pgrep slmain.native` ;
  if [ "$CPID" != "" ] 
  then 
    gdb -p $CPID -batch -n -q -ex 'b caml_call_gc' -ex 'c' -ex 'bt' > bts/$I.bt 2> /dev/null ;
    I=$((I+1)) 
  fi ;
done