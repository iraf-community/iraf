#! /bin/csh
# CL.CSH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.

#set	echo
set	MACH = convex
#set	MACH = `mach`		# SUNOS specific.

# Determine IRAF root directory (value set in install script).
if ($?iraf == 0) then
    setenv iraf "/iraf/iraf/"
endif

# Check for obsolete IRAFBIN definition.
if ($?IRAFBIN && !($?IRAFARCH)) then
    echo "Use IRAFARCH rather than IRAFBIN to specify the machine architecture"
    echo "IRAFARCH, if defined, should be one of ffpa,f68881,i386,sparc, etc."
endif

# Just run the CL if IRAFARCH already defined.
if ($?IRAFARCH) then
    if ($IRAFARCH == "") then
	setenv arch ""
    else
	setenv arch ".$IRAFARCH"
    endif

    setenv IRAFBIN ${iraf}bin$arch/
    set file = ${IRAFBIN}cl.e
    if (-e $file) then
	exec $file
    else
	echo "$file not found"
    endif
endif

# IRAFARCH not defined: determine the architecture to be used.
if ("$MACH" == "convex") then
    if (-e ${iraf}bin.ieee/cl.e) then
	setenv IRAFARCH "ieee"
    else
	setenv IRAFARCH "native"
else if ("$MACH" == "sparc") then
    setenv IRAFARCH "sparc"
else if ("$MACH" == "i386") then
    setenv IRAFARCH "i386"
else if (-e /dev/fpa && -e ${iraf}bin.ffpa/cl.e) then
    setenv IRAFARCH "ffpa"
else
    setenv IRAFARCH "f68881"
endif

setenv arch ".$IRAFARCH"
setenv IRAFBIN ${iraf}bin$arch/
set file = ${IRAFBIN}cl.e

# Run the desired CL.
exec $file
