#!/bin/csh
# FC.CSH -- Link an IMFORT or host fortran program from IRAF.  A front end
# to XC, the purpose of this script is to determine the IRAF architecture
# and add the appropriate host compiler file to XC.

# set	echo

# Determine platform architecture.
if (-f /etc/redhat-release) then
    set MACH = redhat
else
    set MACH = `uname -s | tr '[A-Z]' '[a-z]'`
endif

# Scan the argument list and concatenate all arguments.
set args = ""
while ("$1" != "")
    set args = "$args $1"
    shift
end

# Determine the desired architecture.
if (! $?IRAFARCH) then
    if ("$MACH" == "convex") then
	if (-e ${iraf}bin.ieee/cl.e) then
	    setenv IRAFARCH "ieee"
	else
	    setenv IRAFARCH "native"
	endif
    else if ("$MACH" == "freebsd") then
	setenv IRAFARCH "freebsd"
    else if ("$MACH" == "linux") then
	setenv IRAFARCH "linux"
    else if ("$MACH" == "redhat") then
	setenv IRAFARCH "redhat"
    else if ("$MACH" == "ssol") then
	setenv IRAFARCH "ssun"
    else if ("$MACH" == "sparc") then
	setenv IRAFARCH "sparc"
    else if ("$MACH" == "i386") then
	setenv IRAFARCH "i386"
    else if (-e /dev/fpa && -e ${iraf}bin.ffpa/cl.e) then
	setenv IRAFARCH "ffpa"
    else
	setenv IRAFARCH "f68881"
    endif
endif

# Get float option switch.
switch ($IRAFARCH)
case ieee:
    set float = "-/fi"
    breaksw
case native:
    set float = "-/fn"
    breaksw
case f68881:
    set float = "-/f68881"
    breaksw
case ffpa:
    set float = "-/ffpa"
    breaksw
default:
    set float = ""
    breaksw
endsw

# Call XC with the appropriate float option.
xc $float $args
