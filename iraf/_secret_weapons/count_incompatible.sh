#!/bin/sh

LOG_FILE=$1

less $LOG_FILE | grep 'incompatible pointer type' | sed -e 's/.*`//' -e 's/'"'"'.*//' |sort|awk '{if(a!=$1){printf("%d %s\n",cnt+1,a); a=$1; cnt=0; }else{ cnt++;} }' | sort -n

