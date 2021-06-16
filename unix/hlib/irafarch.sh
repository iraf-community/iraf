#!/bin/sh 
#
#  IRAFARCH -- Determine or set the current platform architecture parameters.
#
#  Usage:       irafarch
#		irafarch -set [<arch>] [opts]
#               irafarch [ -hsi | -nbits | -endian ]
#
#	-mach		print the iraf architecture name [default]
#	-hsi		print the HSI arch
#	-nbits		print number of bits in an int (32 or 64)
#	-endian		print endianness of platform (big or little)
#
#	-actual		print actual architecture name regardless of IRAFARCH
#	-set <arch>	manually reset the iraf environment architecture
#
# ----------------------------------------------------------------------------


##############################################################################
# START OF MACHDEP DEFINITIONS.
##############################################################################

debug=0

# Get the Utility aliases.
# Initialize the $iraf and environment.
if [ -z "$iraf" ]; then
  bindir=$(dirname "$0")                # get iraf root directory 
  iraf=${bindir%/*}/../
fi
. "${iraf}/unix/hlib/util.sh"


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

export MNAME=$($uname_cmd | tr '[:upper:]' '[:lower:]')

# Allow an IRAFARCH definition in the environment to override.

if [ $# -gt 0 ]; then
    if [ "$1" = "-actual" ]; then
        unset IRAFARCH
	shift
    elif [ "$1" = "-current" ]; then
        export IRAFARCH=$(ls -lad "$iraf/bin" | \
			awk '{ printf ("%s\n", $11) }' | \
			sed -e 's/bin.//g')
	shift
    elif [ "$1" = "-set" ]; then
	export IRAFARCH=$2
    fi
fi


# Set some common defaults for most platforms

if [ $debug = 1 ]; then				# DEBUG PRINT
    if [ -n "$IRAFARCH" ]; then
        ECHO " IRAFARCH=$IRAFARCH"
    fi
    ECHO "    MNAME=$MNAME"
fi


# Determine parameters for each architecture.
if [ -n "$IRAFARCH" ]; then
    mach="$IRAFARCH"
    if [ "$mach" = "macintel" ] || [ "$mach" = "macos64" ] || [ "$mach" = "freebsd64" ] || [ "$mach" = "linux64" ]; then
	nbits=64
    else
	nbits=32
    fi
else
    nbits=$(getconf LONG_BIT)
    case "$MNAME" in
     "darwin")		# Mac OS X
	 if [ "$nbits" = 64 ] ; then
	     if [ "$(uname -m)" = "x86_64" ] ; then
	         mach="macintel"
             else
                 mach="macos64"
	     fi
	 else
             mach="macosx"
         fi
         ;;

     "linux")
	 if [ "$nbits" = 64 ] ; then
             mach="linux64"
	 else
	     mach="linux"
         fi
         ;;

     "*freebsd")
	 if [ "$nbits" = 64 ] ; then
             mach="freebsd64"
         else
             mach="freebsd"
         fi
         ;;
     "gnu"|"hurd") # GNU HURD
         mach="hurd"
         ;;

    *)
	ECHO  "Unable to determine platform architecture for ($MNAME)."
	exit 1
	;;
    esac
fi
if [ "$(printf 'I' | od -to2 | awk 'FNR==1{ print substr($2,6,1)}')" = "0" ] ; then
    endian="big"
else
    endian="little"
fi

##############################################################################
# END OF MACHDEP DEFINITIONS.
##############################################################################

# Handle any command-line options.
if [ $# = 0 ]; then
    ECHO $mach
else
    case "$1" in
    "-mach"|"-hsi")
	ECHO "$mach"
	;;
    "-nbits")
	ECHO "$nbits"
	;;
    "-endian")
	ECHO "$endian"
	;;
    "-set")
	if [ -n "$2" ]; then
	    export IRAFARCH=$2
	fi
	export MNAME=$IRAFARCH
	;;
    *)
	ECHO 'Invalid option '"$1"
	;;
    esac
fi

