#!/bin/sh
# MKFLOAT.SH -- Install the indicated version of the IRAF binaries, i.e.,
# archive the current objects and libraries, set BIN to point to bin.FFF,
# and set mkpkg to produce FFF binaries (FFF = f68881, ffpa, sparc, etc.).
#
# NOTE -- This script should be run only by the IRAF system manager.  It is
# assumed that the environment variables defined in the IRAF .bashrc and in
# hlib/irafuser.sh are defined.


ARCH="$1"
DIRS="lib pkg sys"
FILE="unix/hlib/mkpkg.inc"
DFL="_DFL.mkfloat"
TFL="_TFL.mkfloat"

# Set the following to -xpf for BSD Tar and to -xof for SYSV Tar.
TARXFLGS="-xpf"
#TARXFLGS="-xof"

# set echo

# See if we're able to compress the files.
do_compress="1"
if [ ! -x "`which compress`" ]; then
    if [ ! -x "`which gzip`" ]; then
        echo "no compress command found, OBJS.arc files will not be compressed"
	do_compress="0"
    else
	COMPRESS="gzip -S .Z"
    fi
else
    COMPRESS="`which compress`"
fi

# Check for an error in the package structure, i.e. the 'bin' is a directory
# and not a symlink we can change.  It's valid for an external package to
# have only a 'bin' directory, but then it's toplevel mkpkg shouldn't be
# calling us.
if [ "`ls -l bin | grep 'l.........'`" = "" ]; then
    echo "'bin' is a directory, should be symbolic link pointing to valid"
    echo "architecture.  Possible error in copying package structure??"
    exit 1
else
    float="`ls -l bin | sed -e 's+^.*bin\.++'`"
fi
if [ "$ARCH" = "" ]; then
    echo "system is configured for $float"
    exit 0
elif [ "$float" = "$ARCH" ]; then
    echo "system is already configured for $ARCH"
    exit 0
elif [ ! -e bin.$ARCH ]; then
    echo "must set up a bin.$ARCH subdirectory first"
    exit 1
fi

# Get the list of directories to be changed.
shift
if [ "$1" = "-d" ]; then
    DIRS=""
    shift
    while [ ! "$1" = "" ]; do
	DIRS="$DIRS $1"
	shift
    done
fi

echo -n "deleting any dreg .e files left lying about in the source directories... "
rmbin -n -o .a .o .e .E $DIRS > $TFL
grep '\.[eE]$' $TFL | tee _.e_files
rm -f `cat _.e_files` _.e_files
grep -v '\.[eE]$' $TFL > $DFL
rm $TFL
echo "done"

echo -n "archiving and deleting $float objects... "
if [ -e bin.$float ]; then
    if [ ! -z $DFL ]; then
	tar -cf bin.$float/OBJS.arc `cat $DFL`
	tar -tf bin.$float/OBJS.arc | grep -v '/$' | cut -d " " -f 1 > $TFL
	cmp -s $DFL $TFL
	if [ ! $? = 0 ]; then
	    echo "Error: cannot archive $float objects"
	    diff $DFL $TFL
	    rm $DFL $TFL bin.$float/OBJS.arc
	    exit 1
	elif [ "$do_compress" = "1" ]; then
	    echo "done"
	    echo -n "compressing bin.$float/OBJS.arc "
	    nice $COMPRESS -f bin.$float/OBJS.arc &
	    rm -f $TFL
	fi
    fi
else
    echo "old objects will not be archived as no bin.$float directory found"
fi
rm -f `cat $DFL` $DFL

if [ ! "$ARCH" = "generic" ]; then
    echo -n "restoring archived $ARCH objects... "
    if [ -e bin.$ARCH/OBJS.arc.Z ]; then
	zcat bin.$ARCH/OBJS.arc.Z | tar $TARXFLGS -
	if [ $? = 0 ]; then
	    rm -f bin.$ARCH/OBJS.arc.Z
	fi
	echo "done"
    elif [ -e bin.$ARCH/OBJS.arc.gz ]; then
	cat bin.$ARCH/OBJS.arc.gz | gunzip | tar $TARXFLGS -
	if [ $? = 0 ]; then
	    rm -f bin.$ARCH/OBJS.arc.gz
	fi
	echo "done"
    elif [ -e bin.$ARCH/OBJS.arc ]; then
	cat bin.$ARCH/OBJS.arc | tar $TARXFLGS -
	if [ $? = 0 ]; then
	    rm -f bin.$ARCH/OBJS.arc
	fi
	echo "done"
    else
	echo "no object archive found; full sysgen will be needed"
    fi
fi

# Set BIN to point to new directory.
rm -f bin
ln -s bin.$ARCH bin

# If script is run at IRAF root, edit mkpkg.inc for new float option.
#if [ -e $FILE ]; then
#    sed -e "s+= $float+= $ARCH+" $FILE > temp
#    mv -f temp $FILE
#fi

# Warn the user if the new ARCH does not match their current IRAFARCH.
if [ ! "$IRAFARCH" = "" ]; then
    if [ "$ARCH" != "$IRAFARCH" -a "$ARCH" != "generic" ]; then
	echo "Warning: IRAFARCH is still set in your environment to $IRAFARCH"
    fi
fi

