#! /bin/csh
# CL.CSH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.

#set	echo

# Determine platform architecture.
if ($?IRAFARCH) then
    if (-e $iraf/bin.${IRAFARCH}/cl.e) then
	set MACH = $IRAFARCH
    endif
endif
if (! $?MACH) then
    if (-e $iraf/bin.linux/cl.e) then
	set MACH = linux
    else if (-e $iraf/bin.linuz/cl.e) then
	set MACH = linuz
    else
	echo "cannot find $iraf/bin.xxx/cl.e!"
	exit 1
    endif
endif

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

# Determine the architecture to be used.
if ("$MACH" == "linux") then
    setenv IRAFARCH "linux"
else if ("$MACH" == "linuz") then
    setenv IRAFARCH "linuz"
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

setenv arch .$IRAFARCH
setenv IRAFBIN ${iraf}bin$arch/
set file = ${IRAFBIN}cl.e

# Run the desired CL.
exec $file
