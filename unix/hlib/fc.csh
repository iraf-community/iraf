#!/bin/csh -f 
#
# FC.CSH -- Link an IMFORT or host fortran program from IRAF.  A front end
# to XC, the purpose of this script is to determine the IRAF architecture
# and add the appropriate host compiler file to XC.

# set	echo

# Scan the argument list and concatenate all arguments.
set args = ""
while ("$1" != "")
    set args = "$args $1"
    shift
end

# Determine the desired architecture.
setenv IRAFARCH   `$iraf/unix/hlib/irafarch.csh`
setenv MACH       $IRAFARCH

# Get float option switch.
switch ($IRAFARCH)
case macosx:
    set float = "-/arch -//i386"
    breaksw
case macint:
    set float = "-/arch -//x86_64"
    breaksw
case linux64:
    set float = "-/m64"				# FIXME
    breaksw
default:
    set float = ""
    breaksw
endsw

# Call XC with the appropriate float option.
xc $float $args
