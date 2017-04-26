#!/bin/csh -f
#
# CL.CSH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.  This script can be used to invoke a number of CL flavors 
# depending on how it is called.  The install script will create a 'cl'
# and 'ecl' command link to this script with the intent that a different
# binary would be started for each command.  


# Determine CL binary to run based on how we were called.

set cl_binary		= "cl.e"

if (`echo $0 | egrep ecl` != "") then
    set cl_binary	= "ecl.e"
else if ($#argv > 0) then
    if ("$argv[1]" == "-ecl" || "$argv[1]" == "-e") then
        set cl_binary	= "ecl.e"
    endif
else if ($#argv > 0) then
    if ("$argv[1]" == "-old" || "$argv[1]" == "-o") then
        set cl_binary	= "cl.e"
    endif
endif


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

# Determine platform architecture.
if ($?IRAFARCH) then
    if (-e $iraf/bin.${IRAFARCH}/${cl_binary}) then
	set MACH = $IRAFARCH
    endif
endif

if (! $?MACH) then
    if (-f /etc/redhat-release) then
	if (`uname -m` == "ppc") then
	    setenv mach linuxppc
	else
	    setenv mach redhat
	endif
    else if (-f /etc/SuSE-release) then
	set mach = suse
    else
	set mach = `uname -s | tr '[A-Z]' '[a-z]'`
    endif

    if ($mach == "darwin") then
	set mach = macosx
    endif

    if (-e $iraf/bin.$mach/$cl_binary) then
	set MACH = $mach
    else if (-e $iraf/bin.freebsd/$cl_binary) then
	set MACH = freebsd
    else if (-e $iraf/bin.macosx/$cl_binary) then
	set MACH = macosx
    else if (-e $iraf/bin.linux/$cl_binary) then
	set MACH = linux
    else if (-e $iraf/bin.redhat/$cl_binary) then
	set MACH = redhat
    else if (-e $iraf/bin.suse/$cl_binary) then
	set MACH = suse
    else if (-e $iraf/bin.linuxppc/$cl_binary) then
	set MACH = linuxppc
    else if (-e $iraf/bin.sunos/$cl_binary) then
	set MACH = sunos
    else if (-e $iraf/bin.linuz/$cl_binary) then
	set MACH = linuz
    else
	echo "cannot find $iraf/bin.xxx/$cl_binary!"
	exit 1
    endif
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

    # Recent linux systems display a problem in how pointer addresses 
    # interact with the stack and can result in a segfault.  Remove the
    # stacksize limit for IRAF processes until this is better understood.
    if ("$IRAFARCH" == "redhat" || \
        "$IRAFARCH" == "linux" || \
        "$IRAFARCH" == "linuxppc" || \
        "$IRAFARCH" == "suse") then
	    limit stacksize unlimited
    endif

    setenv IRAFBIN ${iraf}bin$arch/
    set file = ${IRAFBIN}$cl_binary
    if (-e $file) then
	exec $file
    else
	echo "$file not found"
    endif
endif


# Determine the architecture to be used.
if ("$MACH" == "freebsd") then
    setenv IRAFARCH "freebsd"
else if ("$MACH" == "linux") then
    setenv IRAFARCH "linux"
else if ("$MACH" == "redhat") then
    setenv IRAFARCH "redhat"
else if ("$MACH" == "suse") then
    setenv IRAFARCH "suse"
else if ("$MACH" == "linuxppc") then
    setenv IRAFARCH "linuxppc"
else if ("$MACH" == "macosx") then
    setenv IRAFARCH "macosx"
else if ("$MACH" == "sunos") then
    setenv IRAFARCH "sunos"
else if ("$MACH" == "linuz") then
    setenv IRAFARCH "linuz"
endif

# Recent linux systems display a problem in how pointer addresses 
# interact with the stack and can result in a segfault.  Remove the
# stacksize limit for IRAF processes until this is better understood.
if ("$IRAFARCH" == "redhat" || \
    "$IRAFARCH" == "linux" || \
    "$IRAFARCH" == "linuxppc" || \
    "$IRAFARCH" == "suse") then
	limit stacksize unlimited
endif

setenv arch .$IRAFARCH
setenv IRAFBIN ${iraf}bin$arch/
set file = ${IRAFBIN}$cl_binary

# Run the desired CL.
exec $file
