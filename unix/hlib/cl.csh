#! /bin/csh
# CL.CSH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.

# Determine platform architecture.
setenv  MACH    `uname -m`

# Determine IRAF root directory (value set in install script).
set d_iraf = "/iraf/iraf/"
if ($?iraf) then
    if (! -e $iraf) then
        echo "Warning: iraf=$iraf does not exist (check .cshrc or .login)"
        echo "Session will default to iraf=$d_iraf"
        unsetenv iraf ; sleep 3
    endif
endif
if ($?iraf == 0) then
    setenv iraf "$d_iraf"
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
if ("$MACH" == "alpha") then
    setenv IRAFARCH "alpha"
else
    setenv IRAFARCH "alpha"
endif

setenv arch .$IRAFARCH
setenv IRAFBIN ${iraf}bin$arch/
set file = ${IRAFBIN}cl.e

# Run the desired CL.
exec $file
