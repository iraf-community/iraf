#! /bin/csh
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

unalias	rm set find echo sleep tail sed cmp echo cat mail

set	helpfile = "${iraf}doc/help.log"
set	arcfile	= "/u1/ftp/iraf/v212/help.log"
set	tmpfile	= "/tmp/help."
set	lokfile = "/tmp/help.lok"

set	number	= 1
set	keywords = ""
set	from	= "`whoami`"
set	date	= "`date`"
#set	irafmail = "iraf@iraf.noao.edu valdes"
set	irafmail = "sites@tucana.tuc.noao.edu valdes"
set	newsgroup = "$1"

# Get exclusive access to the helpfile.

if (-e $lokfile) then
    find $helpfile -newer $lokfile -exec rm -f $lokfile \;
    while (-e $lokfile)
	echo "waiting for access to system helpfile"
	sleep 15
    end
endif

date > $lokfile
onintr cleanup

# If we were called as "helplog -e", simply edit the locked helpfile.

if ("$1" == "-e") then
    vi + $helpfile
    goto cleanup
endif

# Increment the help record number.

set number = "`grep '^NUMBER:' $helpfile | tail -1 | sed -e 's/^NUMBER:.//'`"
if ("$number" == "") then
    set number = 1
else
    set number = "`expr $number + 1`"
endif

# Format new help entry in a temporary file and edit it.

set tmpfile = $tmpfile$number
if (-e $tmpfile) then
    echo "file $tmpfile already exists"
    rm -i $tmpfile
    if (-e $tmpfile) then
	goto edithelp
    endif
endif

echo "NUMBER:	$number" >> $tmpfile
echo "KEYWORDS:	$keywords" >> $tmpfile
echo "DATE:	$date"   >> $tmpfile
echo "FROM:	$from"   >> $tmpfile
echo ""		         >> $tmpfile
echo "Q:	..."     >> $tmpfile
echo ""		         >> $tmpfile
echo "A:	..."     >> $tmpfile

edithelp:
cp $tmpfile $tmpfile.ORIG
vi $tmpfile

# Add new help entry to helpfile (exiting the editor without modifying the file
# causes the help to be discarded).

cmp -s $tmpfile $tmpfile.ORIG
if ($status) then
    while ($newsgroup != "system" && $newsgroup != "applications" && $newsgroup != "none")
	echo -n "Newsgroup (none|system|applications): "
	set newsgroup = "$<"
    end
    set keywords = "`grep '^KEYWORDS:' $tmpfile | tail -1 | sed -e 's/^KEYWORDS:.//'`"
    echo "" >> $helpfile;  cat $tmpfile >> $helpfile
    echo "" >> $arcfile;  cat $tmpfile >> $arcfile
    mail -s "helplog.$number"": $keywords" $irafmail < $tmpfile
    if ($newsgroup != "none") then
        mail -s "helplog.$number"": $keywords"\
	    adass-iraf-{$newsgroup}@iraf.noao.edu < $tmpfile
    endif
    rm -f $tmpfile $tmpfile.ORIG
else
    echo "system helpfile not modified"
    rm -f $tmpfile $tmpfile.ORIG
endif

# Cleanup (vector here on interrupt).

cleanup:
if (-e $lokfile) then
    rm -f $lokfile
endif
