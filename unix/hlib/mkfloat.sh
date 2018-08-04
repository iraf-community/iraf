#!/bin/sh
#
# MKFLOAT.SH -- Install the indicated version of the IRAF binaries, i.e.,
# archive the current objects and libraries, BIN to point to bin.FFF,
# and mkpkg to produce FFF binaries (FFF=f68881, ffpa, sparc, etc.).
#
# NOTE -- This script should be run only by the IRAF system manager.  It is
# assumed that the environment variables defined in the IRAF .login and in
# hlib/irafuser.csh are defined.


ARCH="$1"
DIRS="lib pkg sys"
DFL=_DFL.mkfloat
TFL=_TFL.mkfloat

os_mach=$(uname -s | tr '[:upper:]' '[:lower:]' | cut -c1-6)


# Set the following to -xpf for BSD Tar and to -xof for SYSV Tar.
TARXFLGS=-xpf
#TARXFLGS=-xof

# See if we're able to compress the files.
do_compress=1
if [ ! -x "$(which compress)" ] || [ "$os_mach" = "cygwin" ]; then
    if [ ! -x "$(which gzip)" ]; then
        echo "no compress command found, files will not be compressed"
	do_compress=0
    else
	COMPRESS="gzip -S .Z"
    fi
else
    COMPRESS="compress"
fi

# Check for an error in the package structure, i.e. the 'bin' is a directory
# and not a symlink we can change.  It's valid for an external package to
# have only a 'bin' directory, but then it's toplevel mkpkg shouldn't be
# calling us.
if [ "$(ls -l bin | grep 'l.........')" = "" ]; then
    echo "'bin' is a directory, should be symbolic link pointing to valid"
    echo "architecture.  Possible error in copying package structure??"
    echo "Use tar to copy and move directories to preserve links."
    exit 1
else
    float=$(ls -l bin | sed -e 's+^.*bin\.++')
fi
if [ "$ARCH" = "" ]; then
    echo "system is configured for $float"
    exit 0
elif [ "$float" = "$ARCH" ]; then
    echo "system is already configured for $ARCH"
    exit 0
elif [ ! -e "bin.$ARCH" ]; then
    echo "must up a bin.$ARCH subdirectory first"
    exit 1
fi

# Get the list of directories to be changed.
shift
DIRS=""
if [ "$1" = "-d" ]; then
    DIRS=""
    shift
    while : ; do
	DIRS="$DIRS $1"
	shift
    done
fi

printf \
"deleting any dreg .e files left lying about in the source directories... "
rmbin -n -o .a .o .e .E $DIRS > $TFL;  grep '\.[eE]$' $TFL | tee _.e_files
rm -f $(cat _.e_files) _.e_files; grep -v '\.[eE]$' $TFL > $DFL; rm $TFL
echo "done"

printf "archiving and deleting %s objects... " "$float"
if [ -e "bin.$float" ]; then
    if [ -s "$DFL" ]; then
	tar -cf "bin.$float/OBJS.arc" $(cat "$DFL")
	tar -tf "bin.$float/OBJS.arc" | grep -v '/$' | cut -d " " -f 1 > $TFL
	if cmp -s $DFL $TFL; then
	    echo "Error: cannot archive $float objects"
	    diff "$DFL" "$TFL"
	    rm "$DFL" "$TFL" "bin.$float/OBJS.arc"
	    exit 1
	elif [ "$do_compress" -gt 0 ]; then
	    echo "done"
	    printf "compressing %s" "bin.$float/OBJS.arc"
	    nice "$COMPRESS" -f "bin.$float/OBJS.arc" &
	    rm -f "$TFL"
	fi
    fi
else
    echo "old objects will not be archived as no bin.$float dir found"
fi
echo "done."
rm -f $(cat "$DFL") "$DFL"

if [ "$ARCH" != "generic" ]; then
    printf "restoring archived %s objects... " "$ARCH"
    if [ -e "bin.$ARCH/OBJS.arc.Z" ]; then
	if zcat "bin.$ARCH/OBJS.arc.Z" | tar $TARXFLGS - ; then
	    rm -f "bin.$ARCH/OBJS.arc.Z"
	fi
	echo "done"
    elif [ -e "bin.$ARCH/OBJS.arc.gz" ]; then
	if cat "bin.$ARCH/OBJS.arc.gz" | gunzip | tar $TARXFLGS - ; then
	    rm -f "bin.$ARCH/OBJS.arc.gz"
	fi
	echo "done"
    elif [ -e "bin.$ARCH/OBJS.arc" ]; then
	if cat "bin.$ARCH/OBJS.arc" | tar $TARXFLGS - ; then
	    rm -f "bin.$ARCH/OBJS.arc"
	fi
	echo "done"
    else
	echo
	echo "no object archive found; full sysgen will be needed"
    fi
fi

# Set BIN to point to new directory.
rm -f bin; ln -s "bin.$ARCH" bin


# Warn the user if the new ARCH does not match their current IRAFARCH.
if [ -n $IRAFARCH ]; then
    if [ "$ARCH" != "$IRAFARCH" ]; then
	echo "Warning: IRAFARCH is still in your environment to $IRAFARCH"
    fi
    if [ "$ARCH" = "generic" ]; then
	echo "Warning: IRAFARCH is still in your environment to 'generic'"
    fi
fi
