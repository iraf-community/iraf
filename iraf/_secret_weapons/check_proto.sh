#!/bin/sh

LANG=C

DIR_FUNCS="sys"
DIR_TARGETS="sys"
#DIR_TARGETS="sys pkg tables noao"
#DIR_TARGETS="tables"

FILE_LIST=`find $DIR_FUNCS | grep -v '\.svn' | grep '\.x$'`
#echo flielist = $FILE_LIST

FUNC_LIST=`grep '^[	]*[a-z][a-z_]*[ 	]procedure' $FILE_LIST | tr '\t' ' ' | sed -e 's/^.*://' -e 's/ procedure /%/g' -e 's/(.*//'`

#echo funclist = $FUNC_LIST

for i in $DIR_TARGETS; do
  FILE_LIST=`find $i | grep -v '\.svn' | grep '\.[g]*x$'`
  echo '########' TARGET = $i '########'
  for j in $FUNC_LIST; do
    TYPE=`echo $j | sed -e 's/%.*//'`
    NAME=`echo $j | sed -e 's/^.*%//'`
    echo '====' $TYPE $NAME '===='
    grep -e '^[a-zA-Z][a-zA-Z]*[ 	]' $FILE_LIST | grep -v -e ':'$TYPE'[^a-z_]' -e ':extern[^a-z_]' -e ':errchk[^a-z_]' | grep -e '[^a-zA-Z_]'$NAME'[ 	]*([ 	]*)'
    #for k in $FILE_LIST; do
    #  R=`cat $k | grep '[^a-z_]'$NAME'[ ]*([ ]*)' | grep -v '^'$TYPE'[^a-z_]'`
    #  if [ "$R" != "" ]; then
    #    echo "$k : $R"
    #  fi
    #done
  done
done
