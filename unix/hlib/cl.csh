#! /bin/csh
# CL.CSH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.

#set	echo
#set	MACH = vax
#set	MACH = `mach`		# SUNOS specific.
if ("$ISP" == "") then
    set MACH = "m68k"
else
    set	MACH = `echo $ISP`	# Domain/OS specific (m68k or a88k arch.)
endif

# Determine IRAF root directory (value set in install script).
if ($?iraf == 0) then
    setenv iraf "/usr/iraf/"
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
if ("$MACH" == "sparc") then
    setenv IRAFARCH "sparc"
else if ("$MACH" == "i386") then
    setenv IRAFARCH "i386"
else if ("$MACH" == "m68k") then
    set fpstat =\
	`/etc/nodestat -c | fgrep 'Floating Point Accelerator Unit present'`
    if ("$fpstat" != "" && -e ${iraf}bin.m68k_fpa/cl.e) then
	setenv IRAFARCH "m68k_fpa"
    else
	setenv IRAFARCH "m68k_f68"
    endif
else if ("$MACH" == "a88k") then
    setenv IRAFARCH "a88k"
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
