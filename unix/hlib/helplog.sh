#!/bin/bash
#
# HELPLOG -- Format, edit, and log a new help digest to the system help file.
# 
# Usage:
# 
# helplog [none|system|applications]	log a new help digest
# helplog -e				edit the helpfile (with locking)
# 
# The system helpfile is locked for exclusive access while a help digest is
# being logged, or while the helpfile is being edited.  Logging can be
# aborted either by typing <ctrl/c>, or by editing the editor with ":q!"
# (i.e., by exiting the editor without modifying the temporary file being
# edited).  Help logs are formatted and edited in a small temporary file in
# /tmp and are added at the end of the helpfile only if the task is not
# aborted and the edit modifies the input help log template.  To go back and
# edit a previously logged help digest use "helplog -e".
# 
# Record Format:
# 
# NUMBER:	record number, decimal, sequential.
# KEYWORDS:	search keywords (eg task/package, category)
# DATE:		date help digest logged, unix format date string
# FROM:		user login name
# Q:		digest of question
# A:		digest of answer
# 
# New records are added to the tail of the bugfile.  Left justify field labels,
# indent text to the first tab stop, one blank line between bug entries.
# 
# ----------------------------------------------------------------------------

iraf="`/bin/echo ${iraf}/ | tr -s '/'`"

helpfile="${iraf}doc/help.log"
arcfile="/u1/ftp/iraf/v216/help.log"
tmpfile="/tmp/help."
lokfile="/tmp/help.lok"

number=1
keywords=""
from="`whoami`"
date="`date`"
#irafmail="iraf@iraf.noao.edu valdes"
irafmail="sites@tucana.tuc.noao.edu valdes"
newsgroup="$1"

# Cleanup (vector here on interrupt).

cleanup() {
    if [ -e $lokfile ]; then
	rm -f $lokfile
    fi
    exit 0
}

# Get exclusive access to the helpfile.

if [ -e $lokfile ]; then
    find $helpfile -newer $lokfile -exec rm -f $lokfile \;
    while [ -e $lokfile ]; do
	/bin/echo "waiting for access to system helpfile"
	sleep 15
    done
fi

date > $lokfile
trap cleanup 2

# If we were called as "helplog -e", simply edit the locked helpfile.

if [ "$1" = "-e" ]; then
    vi + $helpfile
    cleanup
fi

# Increment the help record number.

number="`grep '^NUMBER:' $helpfile | tail -1 | sed -e 's/^NUMBER:.//'`"
if [ "$number" = "" ]; then
    number=1
else
    number="`expr $number + 1`"
fi

# Format new help entry in a temporary file and edit it.

SKP=0
tmpfile=$tmpfile$number
if [ -e $tmpfile ]; then
    /bin/echo "file $tmpfile already exists"
    rm -i $tmpfile
    if [ -e $tmpfile ]; then
	SKP=1
    fi
fi

if [ $SKP = 0 ]; then
    /bin/echo "NUMBER:	$number" >> $tmpfile
    /bin/echo "KEYWORDS:	$keywords" >> $tmpfile
    /bin/echo "DATE:	$date"	>> $tmpfile
    /bin/echo "FROM:	$from"	>> $tmpfile
    /bin/echo ""		>> $tmpfile
    /bin/echo "Q:	..."	>> $tmpfile
    /bin/echo ""		>> $tmpfile
    /bin/echo "A:	..."	>> $tmpfile
fi

cp $tmpfile $tmpfile.ORIG
vi $tmpfile

# Add new help entry to helpfile (exiting the editor without modifying the file
# causes the help to be discarded).

cmp -s $tmpfile $tmpfile.ORIG
if [ $? = 0 ]; then
    /bin/echo "system helpfile not modified"
    rm -f $tmpfile $tmpfile.ORIG
else
    while [ $newsgroup != "system" -a $newsgroup != "applications" -a $newsgroup != "none" ]; do
	/bin/echo -n "Newsgroup (none|system|applications): "
	read newsgroup
    done
    keywords="`grep '^KEYWORDS:' $tmpfile | tail -1 | sed -e 's/^KEYWORDS:.//'`"
    /bin/echo "" >> $helpfile;  cat $tmpfile >> $helpfile
    if [ -e $arcfile ]; then
        /bin/echo "" >> $arcfile;  cat $tmpfile >> $arcfile
    fi
    mail -s "helplog.$number"": $keywords" $irafmail < $tmpfile
    if [ $newsgroup != "none" ]; then
        mail -s "helplog.$number"": $keywords"\
	    adass-iraf-{$newsgroup}@iraf.noao.edu < $tmpfile
    fi
    rm -f $tmpfile $tmpfile.ORIG
fi

cleanup

