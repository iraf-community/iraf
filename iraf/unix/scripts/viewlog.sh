#!/bin/sh
cat $1 | awk '{ if (match($0,"^[a-z0-9_][a-z0-9_]*[.][xf]:")) {SRC=$0;} else if (match($0,"^   [a-z0-9_][a-z0-9_]*:")) {FNC=substr($0,4,length($0)-4);} else if(match($0,"^[a-z/]")) {SRC="";print;} else if ($0 != "") {if (SRC != ""){printf("%s In %c%s%c:\n",SRC,96,FNC,39);print;SRC="";} else {print;} }  }'

