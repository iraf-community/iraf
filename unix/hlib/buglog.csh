#! /bin/csh
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

unalias	rm set find echo sleep tail sed cmp echo cat mail

set	bugfile	= "${iraf}doc/bugs.log"
set	arcfile	= "/u1/ftp/iraf/v214/bugs.log"
set	tmpfile	= "/tmp/bug."
set	lokfile = "/tmp/bug.lok"

set	number	= 1
set	module	= "$1"
set	from	= "`whoami`"
set	date	= "`date`"
set	system	= "V2.14"
set	irafmail = "iraf@iraf.noao.edu"
set	buglog   = "adass-iraf-buglog@iraf.noao.edu"

# Get exclusive access to the bugfile.

if (-e $lokfile) then
    find $bugfile -newer $lokfile -exec rm -f $lokfile \;
    while (-e $lokfile)
	echo "waiting for access to system bugfile"
	sleep 15
    end
endif

date > $lokfile
onintr cleanup

# If we were called as "buglog -e", simply edit the locked bugfile.

if ("$1" == "-e") then
    vi + $bugfile
    goto cleanup
endif

# Increment the bug record number.

set number = "`grep '^NUMBER:' $bugfile | tail -1 | sed -e 's/^NUMBER:.//'`"
if ("$number" == "") then
    set number = 1
else
    set number = "`expr $number + 1`"
endif

# Get module name if not given on command line.

if ($module == "") then
    echo -n "Module: "
    set module = "$<"
endif

# Format new bug entry in a temporary file and edit it.

set tmpfile = $tmpfile$number
if (-e $tmpfile) then
    echo "file $tmpfile already exists"
    rm -i $tmpfile
    if (-e $tmpfile) then
	goto editbug
    endif
endif

echo "NUMBER:	$number" >> $tmpfile
echo "MODULE:	$module" >> $tmpfile
echo "SYSTEM:	$system" >> $tmpfile
echo "DATE:	$date"   >> $tmpfile
echo "FROM:	$from"   >> $tmpfile
echo ""		         >> $tmpfile
echo "BUG:	..."     >> $tmpfile
echo ""		         >> $tmpfile
echo "STATUS:	..."     >> $tmpfile

editbug:
cp $tmpfile $tmpfile.ORIG
vi $tmpfile

# Add new bug entry to bugfile (exiting the editor without modifying the file
# causes the bug to be discarded).

cmp -s $tmpfile $tmpfile.ORIG
if ($status) then
    echo "" >> $bugfile;  cat $tmpfile >> $bugfile
    echo "" >> $arcfile;  cat $tmpfile >> $arcfile
    mail -s "buglog.$number"": module = $module, author = $from" $irafmail\
	< $tmpfile
#    mail -s "buglog.$number"": module = $module, author = $from" $buglog\
#	< $tmpfile
    rm -f $tmpfile $tmpfile.ORIG
else
    echo "system bugfile not modified"
    rm -f $tmpfile $tmpfile.ORIG
endif

# Cleanup (vector here on interrupt).

cleanup:
if (-e $lokfile) then
    rm -f $lokfile
endif
