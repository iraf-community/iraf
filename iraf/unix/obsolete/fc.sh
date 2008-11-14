#!/bin/sh
# FC.SH -- Link an IMFORT or host fortran program from IRAF.  A front end
# to XC, the purpose of this script is to determine the IRAF architecture
# and add the appropriate host compiler file to XC.

# set	echo

# Determine platform architecture.
if [ -f /etc/redhat-release ]; then
    if [ "`uname -m`" = "ppc" ]; then
        MACH="linuxppc"
    else
        MACH="redhat"
    fi
elif [ -f /etc/yellowdog-release -o "`uname -m`" = "ppc" ]; then
    MACH="linuxppc"
else
    MACH="`uname -s | tr '[A-Z]' '[a-z]'`"
fi

if [ "$MACH" = "darwin" ]; then
    MACH="macosx"
fi

export MACH

iraf="`echo ${iraf}/ | tr -s '/'`"

# Determine the desired architecture.
if [ "$IRAFARCH" = "" ]; then
    if [ "$MACH" = "convex" ]; then
	if [ -e ${iraf}bin.ieee/cl.e ]; then
	    IRAFARCH="ieee"
	else
	    IRAFARCH="native"
	fi
    elif [ "$MACH" = "freebsd" ]; then
	IRAFARCH="freebsd"
    elif [ "$MACH" = "macosx" ]; then
	IRAFARCH="macosx"
    elif [ "$MACH" = "linux" ]; then
	IRAFARCH="linux"
    elif [ "$MACH" = "redhat" ]; then
	IRAFARCH="redhat"
    elif [ "$MACH" = "linuxppc" ]; then
	IRAFARCH="linuxppc"
    elif [ "$MACH" = "sunos" ]; then
	IRAFARCH="sunos"
    elif [ "$MACH" = "ssol" ]; then
	IRAFARCH="ssun"
    elif [ "$MACH" = "sparc" ]; then
	IRAFARCH="sparc"
    elif [ "$MACH" = "i386" ]; then
	IRAFARCH="i386"
    elif [ -e /dev/fpa -a -e ${iraf}bin.ffpa/cl.e ]; then
	IRAFARCH="ffpa"
    else
	IRAFARCH="f68881"
    fi
fi

export IRAFARCH

# Get float option switch.
case "$IRAFARCH" in
ieee)
    float="-/fi"
    ;;
native)
    float="-/fn"
    ;;
f68881)
    float="-/f68881"
    ;;
ffpa)
    float="-/ffpa"
    ;;
*)
    float=""
    ;;
esac
# Call XC with the appropriate float option.
xc $float $@
