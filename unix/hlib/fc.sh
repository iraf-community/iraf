#!/bin/bash
#
# FC.SH -- Link an IMFORT or host fortran program from IRAF.  A front end
# to XC, the purpose of this script is to determine the IRAF architecture
# and add the appropriate host compiler file to XC.


# Determine the desired architecture.
IRAFARCH=`$iraf/unix/hlib/irafarch.csh`
MACH=$IRAFARCH


# Set any float option switch.
case $IRAFARCH in
    macosx)
        float="-/arch -//i386"
        ;;
    macintel)
        float="-/arch -//x86_64"
        ;;
    linux64)
        float="-/m64"				# FIXME
        ;;
    *)
        float=""
        ;;
esac

# Call XC with the appropriate float option.
xc $float $@
