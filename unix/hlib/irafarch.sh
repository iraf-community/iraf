#!/bin/bash 
#
#  IRAFARCH -- Determine or set the current platform architecture parameters.
#
#  Usage:       irafarch
#		irafarch -set [<arch>] [opts]
#               irafarch [ -hsi | -nbits | -pipe | -tapecap | -tape ]
#
#	-mach		print the iraf architecture name [default]
#	-hsi		print the HSI arch
#	-nbits		print number of bits in an int (32 or 64)
#	-pipe		does platform support display fifo pipes?
#	-tapecap	does platform require tapecap changes?
#	-tape		does platform support tape drives?
#	-shlib		does platform support iraf shared libs?
#
#	-actual		print actual architecture name regardless of IRAFARCH
#	-set <arch>	manually reset the iraf environment architecture
#
# ----------------------------------------------------------------------------


##############################################################################
# START OF MACHDEP DEFINITIONS.
##############################################################################

hmach="INDEF"
nbits=32
pipes=1
shlibs=0
tapecaps=0
tapes=1

debug=0

# Get the Utility aliases.
# Initialize the $iraf and environment.
if [ -z "$iraf" ]; then
  bindir="`dirname $0`"                # get iraf root directory 
  iraf=${bindir%/*}/../
fi
source ${iraf}/unix/hlib/util.sh


#----------------------------------
# Determine platform architecture.
#----------------------------------

if [ -e /usr/bin/uname ]; then
    uname_cmd=/usr/bin/uname
elif [ -e /bin/uname ]; then
    uname_cmd=/bin/uname
else
    WARNING  "No 'uname' command found to determine architecture."
    exit 1
fi

export  UNAME=`$uname_cmd | tr '[A-Z]' '[a-z]'`
if [ $UNAME == "sunos" ]; then
    export  UNAME_M=`$uname_cmd -m | cut -c2- | tr '[A-Z]' '[a-z]'`
else
    export  UNAME_M=`$uname_cmd -m | tr '[A-Z]' '[a-z]' | tr ' ' '_'` 
fi
export  OSVERSION=`$uname_cmd -r | cut -c1`


# Allow an IRAFARCH definition in the environment to override.

_setmname() {
    export MNAME=$1
    export MNAME_M=$2
}

if (( $# > 1 )); then
    if [ "$1" == "-actual" ]; then
	_setmname $UNAME $UNAME_M
        unset IRAFARCH

    elif [ "$1" == "-current" ]; then
        export MNAME=`/bin/ls -lad $iraf/bin | \
			awk '{ printf ("%s\n", $11) }' | \
			sed -e 's/bin.//g'`
        export MNAME_M=$UNAME_M
        export IRAFARCH=$MNAME
	_setmname $IRAFARCH $UNAME_M
    fi
else
    if (( $# == 0 )); then
        if [ -n "$IRAFARCH" ]; then
	    _setmname $IRAFARCH $UNAME_M
        else
	    _setmname $UNAME $UNAME_M
        fi
    else
        if [ "$1" == "-set" ]; then
	    _setmname $2 $2
        else
	    _setmname $UNAME $UNAME_M
        fi
    fi
fi


# Set some common defaults for most platforms
shlib=0				# no shared lib support
nbits=32			# 32-bit architecture
tapecaps=1			# platform supports tapecaps
tapes=1				# platform support tape drives
pipes=1				# supports display fifo pipes

pciraf=1			# PC-IRAF system
suniraf=0			# SUN-IRAF system

if (( $debug == 1 )); then				# DEBUG PRINT
    if [ -n "$IRAFARCH" ]; then
        ECHO " IRAFARCH=$IRAFARCH"
    fi
    ECHO "    MNAME=$MNAME"
    ECHO "  MNAME_M=$MNAME_M"
    ECHO "OSVERSION=$OSVERSION"
fi


# Determine parameters for each architecture.
case "$MNAME" in
     "darwin"|"ipad"|"macosx"|"macintel")		# Mac OS X
        if [ -n "$IRAFARCH" ]; then
            mach="$IRAFARCH"
            hmach="$IRAFARCH"
	    if [ "$mach" == "macintel" ]; then
		nbits=64
	    fi
	else 
            if [ "$MNAME_M" == "x86_64" ]; then		# 64-bit
                mach="macintel"
                hmach="macintel"
		nbits=64
            elif [ "$MNAME_M" == "x86" -o "$MNAME_M" == "i386" ]; then
                mach="macosx"
                hmach="macosx"
		nbits=32
            else
                mach="ipad"				# iOS Device
                hmach="ipad"
		nbits=32
            fi
	fi
	tapecaps=0
	tapes=0
	pipes=0
        ;;

    "redhat"|"linux"|"linux64")
        if [ -n "$IRAFARCH" ]; then
            mach="$IRAFARCH"
            hmach="$IRAFARCH"
	    if [ "$mach" == "linux64" ]; then
		nbits=64
	    fi
	else 
            if [ "$MNAME_M" == "x86_64" ]; then		# Linux x86_64
                mach="linux64"
                hmach="linux64"
	        nbits=64
            else					# Linux
                mach="linux"
                hmach="linux"
	        nbits=32
            fi
        fi
        ;;

    "ssun"|"sparc"|"sunos")
	tapecaps=1
        if [ $UNAME_M != "86pc" ]; then
	    suniraf=1
	    pciraf=0
            if [ $OSVERSION == 5 ]; then			# Sparc Solaris
                mach= "ssun"
                hmach= "ssol"
            else			   		# Sparc SunOS 4.x
                mach="sparc"
                hmach="sparc"
            fi
        else
            mach="sunos"	    	# Intel Solaris x86
            hmach="sunos"
	    tapecaps=0
	    tapes=0
	    pipes=0
        fi
        ;;

    "freebsd") 					# FreeBSD
        mach="freebsd"
        hmach="freebsd"
	tapecaps=0
	tapes=0
	pipes=0
        ;;

    *)
	# We don't want to be limited by the CYGWIN version numbering so
	# look for a truncated match here before punting.
	os_mach=`ECHO $UNAME | cut -c1-6`
	if [ "$os_mach" == "cygwin" ]; then
            mach="cygwin"
            hmach="cygwin"
	    shlib=0
	    tapecaps=0
	    tapes=0
	    pipes=0
	else
	    ECHO  'Unable to determine platform architecture for ($MNAME).'
	    exit 1
	fi
	;;
esac

##############################################################################
# END OF MACHDEP DEFINITIONS.
##############################################################################

# Handle any command-line options.
if (( $# == 0 )); then
    ECHO $mach
else
    case "$1" in
    "-mach")
	ECHO $mach
	;;
    "-actual")
	ECHO $mach
	;;
    "-current")
	ECHO $mach
	;;
    "-hsi")
	ECHO $hmach
	;;
    "-nbits")
	ECHO $nbits
	;;
    "-pipes")
	ECHO $pipes
	;;
    "-tapecap")
	ECHO $tapecaps
	;;
    "-tapes")
	ECHO $tapes
	;;
    "-shlib")
	ECHO $shlib
	;;
    "-set")
	if [ -n $2 ]; then
	    export IRAFARCH=$2
	fi
	_setmname $IRAFARCH $UNAME_M
	;;
    *)
	ECHO 'Invalid option '$1
	;;
    esac
fi

