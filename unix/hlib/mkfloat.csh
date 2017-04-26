#!/bin/csh
#
# MKFLOAT.CSH -- Install the indicated version of the IRAF binaries, i.e.,
# archive the current objects and libraries, set BIN to point to bin.FFF,
# and set mkpkg to produce FFF binaries (FFF = f68881, ffpa, sparc, etc.).
#
# NOTE -- This script should be run only by the IRAF system manager.  It is
# assumed that the environment variables defined in the IRAF .login and in
# hlib/irafuser.csh are defined.


set ARCH = "$1"
set DIRS = "lib pkg sys"
set FILE = unix/hlib/mkpkg.inc
set DFL  = _DFL.mkfloat
set TFL  = _TFL.mkfloat

set mach = `uname -s | tr '[A-Z]' '[a-z]'`
set os_mach = `uname -s | tr '[A-Z]' '[a-z]' | cut -c1-6`

unalias	ls rm cat grep tar cmp diff echo ln mv zcat gunzip compress which
unset noclobber

# Set the following to -xpf for BSD Tar and to -xof for SYSV Tar.
set TARXFLGS = -xpf
#set TARXFLGS = -xof

# set echo

# See if we're able to compress the files.
set do_compress = 1
if (! -x `which compress` || $os_mach == "cygwin") then
    if (! -x `which gzip`) then
        echo "no compress command found, OBJS.arc files will not be compressed"
	set do_compress = 0
    else
	set COMPRESS = "gzip -S .Z"
    endif
else
    set COMPRESS =  `which compress`
endif

# Check for an error in the package structure, i.e. the 'bin' is a directory
# and not a symlink we can change.  It's valid for an external package to
# have only a 'bin' directory, but then it's toplevel mkpkg shouldn't be
# calling us.
if ("`ls -l bin | grep 'l.........'`" == "") then
    echo "'bin' is a directory, should be symbolic link pointing to valid"
    echo "architecture.  Possible error in copying package structure??"
    echo "Use tar to copy and move directories to preserve links."
    exit 1
else
    set float = `ls -l bin | sed -e 's+^.*bin\.++'`
endif
if ("$ARCH" == "") then
    echo "system is configured for $float"
    exit 0
else if ($float == "$ARCH") then
    echo "system is already configured for $ARCH"
    exit 0
else if (! -e bin.$ARCH) then
    echo "must set up a bin.$ARCH subdirectory first"
    exit 1
endif

# Get the list of directories to be changed.
shift
if ("$1" == "-d") then
    set DIRS = ""
    shift
    while ("$1" != "")
	set DIRS = "$DIRS $1"
	shift
    end
endif

echo -n \
"deleting any dreg .e files left lying about in the source directories... "
rmbin -n -o .a .o .e .E $DIRS > $TFL;  grep '\.[eE]$' $TFL | tee _.e_files
rm -f `cat _.e_files` _.e_files; grep -v '\.[eE]$' $TFL > $DFL; rm $TFL
echo "done"

echo -n "archiving and deleting $float objects... "
if (-e bin.$float) then
    if (! -z $DFL) then
	tar -cf bin.$float/OBJS.arc `cat $DFL`
	tar -tf bin.$float/OBJS.arc | grep -v '/$' | cut -d " " -f 1 > $TFL
	cmp -s $DFL $TFL
	if ($status) then
	    echo "Error: cannot archive $float objects"
	    diff $DFL $TFL
	    rm $DFL $TFL bin.$float/OBJS.arc
	    exit 1
	else if ($do_compress == 1) then
	    echo "done"
	    echo -n "compressing bin.$float/OBJS.arc "
	    nice $COMPRESS -f bin.$float/OBJS.arc &
	    rm -f $TFL
	endif
    endif
else
    echo "old objects will not be archived as no bin.$float directory found"
endif
echo ""
rm -f `cat $DFL` $DFL

if ($ARCH != generic) then
    echo -n "restoring archived $ARCH objects... "
    if (-e bin.$ARCH/OBJS.arc.Z) then
	if ({ (zcat bin.$ARCH/OBJS.arc.Z | tar $TARXFLGS -) }) then
	    rm -f bin.$ARCH/OBJS.arc.Z
	endif
	echo "done"
    else if (-e bin.$ARCH/OBJS.arc.gz) then
	if ({ (cat bin.$ARCH/OBJS.arc.gz | gunzip | tar $TARXFLGS -) }) then
	    rm -f bin.$ARCH/OBJS.arc.gz
	endif
	echo "done"
    else if (-e bin.$ARCH/OBJS.arc) then
	if ({ (cat bin.$ARCH/OBJS.arc | tar $TARXFLGS -) }) then
	    rm -f bin.$ARCH/OBJS.arc
	endif
	echo "done"
    else
	echo ""
	echo "no object archive found; full sysgen will be needed"
    endif
endif

# Set BIN to point to new directory.
rm -f bin; ln -s bin.$ARCH bin

# If script is run at IRAF root, edit mkpkg.inc for new float option.
#if (-e $FILE) then
#    sed -e "s+= $float+= $ARCH+" $FILE > temp; mv -f temp $FILE
#endif

# Warn the user if the new ARCH does not match their current IRAFARCH.
if ($?IRAFARCH == 1) then
    if ($ARCH != $IRAFARCH && $ARCH != generic) then
	echo "Warning: IRAFARCH is still set in your environment to $IRAFARCH"
    endif
endif
