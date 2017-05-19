#!/bin/bash
#
# BUGLOG -- Format, edit, and log a new bug to the system bugs file.
#
# Usage:
#
#	buglog [module]		log a new bug to the system bugsfile
#	buglog -e		edit the system bugfile (with locking)
#
# The system bugsfile is locked for exclusive access while a bug is being
# logged, or while the bugsfile is being edited.  Logging can be aborted either
# by typing <ctrl/c>, or by editing the editor with ":q!" (i.e., by exiting
# the editor without modifying the temporary file being edited).  Bugs are
# formatted and edited in a small temporary file in /tmp and are added at the
# end of the bugsfile only if the task is not aborted and the edit modifies
# the input bug template.  To go back and edit a previously logged bug use
# "buglog -e".
#
# Record Format:
#
#	NUMBER:		record number, decimal, sequential.
#	MODULE:		package.task or library.procedure or 'unknown'
#	SYSTEM:		versions of iraf in which bug was present
#	DATE:		date bug logged, unix format date string
#	FROM:		user login name
#	BUG:		description of the bug
#	STATUS:		'fixed in V2.X', 'unresolved', etc.
#
# New records are added to the tail of the bugfile.  Left justify field labels,
# indent text to the first tab stop, one blank line between bug entries.
# ----------------------------------------------------------------------------

iraf="`/bin/echo ${iraf}/ | tr -s '/'`"

bugfile="${iraf}doc/bugs.log"
arcfile="/u1/ftp/iraf/v216/bugs.log"
tmpfile="/tmp/bug."
lokfile="/tmp/bug.lok"

number=1
module="$1"
from="`whoami`"
date="`date`"
system="V2.16"
irafmail="admin@iraf.net"
buglog="adass-iraf-buglog@iraf.noao.edu"

# Cleanup (vector here on interrupt).

cleanup() {
    if [ -e $lokfile ]; then
	rm -f $lokfile
    fi
    exit 0
}

# Get exclusive access to the bugfile.

if [ -e $lokfile ]; then
    find $bugfile -newer $lokfile -exec rm -f $lokfile \;
    while [ -e $lokfile ]; do
	/bin/echo "waiting for access to system bugfile"
	sleep 15
    done
fi

date > $lokfile
trap cleanup 2

# If we were called as "buglog -e", simply edit the locked bugfile.

if [ "$1" = "-e" ]; then
    vi + $bugfile
    cleanup
fi

# Increment the bug record number.

number="`grep '^NUMBER:' $bugfile | tail -1 | sed -e 's/^NUMBER:.//'`"
if [ "$number" = "" ]; then
    number=1
else
    number="`expr $number + 1`"
fi

# Get module name if not given on command line.

if [ "$module" = "" ]; then
    /bin/echo -n "Module: "
    read module
fi

# Format new bug entry in a temporary file and edit it.

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
    /bin/echo "MODULE:	$module" >> $tmpfile
    /bin/echo "SYSTEM:	$system" >> $tmpfile
    /bin/echo "DATE:	$date"	>> $tmpfile
    /bin/echo "FROM:	$from"	>> $tmpfile
    /bin/echo ""		>> $tmpfile
    /bin/echo "BUG:	..."	>> $tmpfile
    /bin/echo ""			>> $tmpfile
    /bin/echo "STATUS:	..."	>> $tmpfile
fi

cp $tmpfile $tmpfile.ORIG
vi $tmpfile

# Add new bug entry to bugfile (exiting the editor without modifying the file
# causes the bug to be discarded).

cmp -s $tmpfile $tmpfile.ORIG
if [ $? = 0 ]; then
    /bin/echo "system bugfile not modified"
    rm -f $tmpfile $tmpfile.ORIG
else
    /bin/echo "" >> $bugfile;  cat $tmpfile >> $bugfile
    if [ -e $arcfile ]; then
        /bin/echo "" >> $arcfile;  cat $tmpfile >> $arcfile
    fi
    mail -s "buglog.$number"": module = $module, author = $from" $irafmail\
	< $tmpfile
#    mail -s "buglog.$number"": module = $module, author = $from" $buglog\
#	< $tmpfile
    rm -f $tmpfile $tmpfile.ORIG
fi

cleanup

