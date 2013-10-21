#!/bin/bash
#
# CL.SH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.  This script can be used to invoke a number of CL flavors 
# depending on how it is called.  The install script will create a 'cl'
# and 'ecl' command link to this script with the intent that a different
# binary would be started for each command.  


# Determine CL binary to run based on how we were called.


nm=${0##*/}
cl_binary="vocl.e"

case "$nm" in
    "cl" | "cl.sh")
        cl_binary="cl.e"
        ;;
    "ecl" | "ecl.sh")
        cl_binary="ecl.e"
        ;;
    "vocl" | "vocl.sh")
        cl_binary="vocl.e"
        ;;
    *)
	if (( $# > 1 )); then
	    if [ $1 == "-old" -o $1 == "-o" ]; then
        	cl_binary="cl.e"
	    elif [ $1 == "-ecl" -o $1 == "-e" ]; then
        	cl_binary="ecl.e"
	    elif [ $1 == "-vo" ]; then
        	cl_binary="vocl.e"
	    elif [ ${1##*.} == "c" ]; then
		# Workaround for autoconf scripts attempting to use this 
		# command as a valid compiler option.  On some systems (mostly
		# Debian) a valid CC command can't be found and eventually 
		# the 'cl' (lisp) compiler is tried.  It will always apparently
		# have the conftest.c test file, so simply exit with a code to
		# tell autoconf it won't work.
		exit 1
	    fi
	fi
esac

# Determine IRAF root directory (value set in install script).
d_iraf="/iraf/iraf/"
if [ -n $iraf ]; then
    if [ ! -e $iraf ]; then
        echo "Warning: iraf=$iraf does not exist (check .cshrc or .login)"
        echo "Session will default to iraf=$d_iraf"
        unset iraf ; sleep 3
    fi
fi
if [ -z $iraf ]; then
    export iraf="$d_iraf"
fi

# Check for a version query.
if [ $# > 1 ]; then
    case "$1" in
	"-v" | "-V" | "-version" | "--version")
            head -1 $iraf/unix/hlib/motd
	    exit 0
	    ;;
	*)
	    ;;
    esac
fi


# Determine platform architecture.
if [ -e $iraf/unix/hlib/irafarch.sh ]; then
    ACTUAL_ARCH=`$iraf/unix/hlib/irafarch.sh -actual`
else
    ACTUAL_ARCH=$IRAFARCH
fi

if [ -n "$IRAFARCH" ]; then
    if [ -e $iraf/bin.${IRAFARCH}/${cl_binary} ]; then
	MACH=$IRAFARCH
    else
        echo "ERROR:  No $iraf/bin.${IRAFARCH}/${cl_binary} binary found."
	if [ "$ACTUAL_ARCH" != "$IRAFARCH" ]; then
            echo "ERROR:  IRAFARCH set to '$IRAFARCH', should be '$ACTUAL_ARCH'"
	fi
	exit 1
    fi
    export arch=".$MACH"

else
    os_mach=`uname -s | tr '[A-Z]' '[a-z]' | cut -c1-6`
 
    if [ -e $iraf/unix/hlib/irafarch.csh ]; then
        MACH=`$iraf/unix/hlib/irafarch.csh`
    else
        MACH=$os_mach
    fi

    if [ "$os_mach" == "linux" ]; then		# handle linux systems
        if [ `uname -m` == "x86_64" ]; then
            export mach="linux64"
        else
            export mach="linux"
        fi
    elif [ "$os_mach" == "darwin" ]; then	# handle Mac systems
        if [ "`uname -m`" == "x86_64" ]; then
            export mach="macintel"
        else
            export mach="macosx"
        fi
    elif [ "$os_mach" == "cygwin" ]; then
        export mach="cygwin"
    else
        mach=`uname -s | tr '[A-Z]' '[a-z]'`
    fi

    export arch=".$MACH"
    if [ -z $IRAFARCH ]; then
        export IRAFARCH="$MACH"
    fi

    if [ ! -e $iraf/bin.${MACH}/${cl_binary} ]; then
        echo "ERROR:  No $iraf/bin.${IRAFARCH}/${cl_binary} binary found."
	exit 1
    fi
fi


# Recent linux systems display a problem in how pointer addresses 
# interact with the stack and can result in a segfault.  Remove the
# stacksize limit for IRAF processes until this is better understood.
if [ "$IRAFARCH" == "redhat" -o \
     "$IRAFARCH" == "linux64" -o \
     "$IRAFARCH" == "linux" ]; then
	ulimit -s unlimited
fi


# Just run the CL if IRAFARCH already defined.
if [ -n "$IRAFARCH" ]; then
    if [ -z $IRAFARCH ]; then
	export arch=""
    else
	export arch=".$IRAFARCH"
    fi

    export IRAFBIN=${iraf}bin$arch/
    file=${IRAFBIN}$cl_binary
    if [ -e $file ]; then
	exec $file
    else
	echo "$file not found"
    fi
fi


# Set the architecture to be used.
export IRAFARCH=$MACH
export arch=.$IRAFARCH
export IRAFBIN=${iraf}bin$arch/

# Run the desired CL.
exec  ${IRAFBIN}$cl_binary
