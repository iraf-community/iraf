#!/bin/csh
#
# CL.CSH -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.  This script can be used to invoke a number of CL flavors 
# depending on how it is called.  The install script will create a 'cl'
# and 'ecl' command link to this script with the intent that a different
# binary would be started for each command.  


# Determine CL binary to run based on how we were called.

set cl_binary		= "vocl.e"

if (`echo $0 | egrep ecl` != "") then
    set cl_binary	= "ecl.e"

else if (`echo $0 | egrep vo` != "") then
    set cl_binary	= "vocl.e"

else if ($#argv > 0) then
    if ("$argv[1]" == "-old" || "$argv[1]" == "-o") then
        set cl_binary	= "cl.e"
    else if ("$argv[1]" == "-vo") then
        set cl_binary	= "vocl.e"
    else if ("$argv[1]:e" == "c") then
	# Workaround for autoconf scripts attempting to use this command as
	# a valid compiler option.  On some systems (mostly Debian) a valid
	# CC command can't be found and eventually the 'cl' (lisp) compiler
	# is tried.  It will always apparently have the conftest.c test file,
	# so simply exit with a code to tell autoconf it won't work.
	exit 1
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

# Check for a version query.
if ($#argv > 0) then
    if ("$argv[1]" == "-v" || "$argv[1]" == "-version" || \
	"$argv[1]" == "-V" || "$argv[1]" == "--version") then
            head -1 $iraf/unix/hlib/motd
	    exit 0
    endif
endif


# Determine platform architecture.
if (-e $iraf/unix/hlib/irafarch.csh) then
    set ACTUAL_ARCH = `$iraf/unix/hlib/irafarch.csh -actual`
else
    set ACTUAL_ARCH = $IRAFARCH
endif

if ($?IRAFARCH) then
    if (-e $iraf/bin.${IRAFARCH}/${cl_binary}) then
	set MACH = $IRAFARCH
    else
        echo "ERROR:  No $iraf/bin.${IRAFARCH}/${cl_binary} binary found."
	if ("$ACTUAL_ARCH" != "$IRAFARCH") then
            echo "ERROR:  IRAFARCH set to '$IRAFARCH', should be '$ACTUAL_ARCH'"
	endif
	exit 1
    endif
    setenv arch ".$MACH"

else
    set os_mach = `uname -s | tr '[A-Z]' '[a-z]' | cut -c1-6`
 
    if (-e $iraf/unix/hlib/irafarch.csh) then
        set MACH = `$iraf/unix/hlib/irafarch.csh`
    else
        set MACH = $os_mach
    endif

    if ("$os_mach" == "linux") then		# handle linux systems
        if (`uname -m` == "x86_64") then
            setenv mach linux64
        else
            setenv mach linux
        endif
    else if ("$os_mach" == "darwin") then	# handle Mac systems
        if ("`uname -m`" == "x86_64") then
            setenv mach macintel
        else
            setenv mach macosx
        endif
    else if ("$os_mach" == "cygwin") then
        setenv mach cygwin
    else
        set mach = `uname -s | tr '[A-Z]' '[a-z]'`
    endif

    setenv arch ".$MACH"
    if (! $?IRAFARCH) then
        setenv IRAFARCH "$MACH"
    endif

    if (! (-e $iraf/bin.${MACH}/${cl_binary}) ) then
        echo "ERROR:  No $iraf/bin.${IRAFARCH}/${cl_binary} binary found."
	exit 1
    endif
endif

# Recent linux systems display a problem in how pointer addresses 
# interact with the stack and can result in a segfault.  Remove the
# stacksize limit for IRAF processes until this is better understood.
if ("$IRAFARCH" == "redhat" || \
    "$IRAFARCH" == "linux64" || \
    "$IRAFARCH" == "linux") then
	limit stacksize unlimited
endif


# Just run the CL if IRAFARCH already defined.
if ($?IRAFARCH) then
    if ($IRAFARCH == "") then
	setenv arch ""
    else
	setenv arch ".$IRAFARCH"
    endif

    setenv IRAFBIN ${iraf}bin$arch/
    set file = ${IRAFBIN}$cl_binary
    if (-e $file) then
	exec $file
    else
	echo "$file not found"
    endif
endif


# Set the architecture to be used.
setenv IRAFARCH   $MACH


setenv arch 	.$IRAFARCH
setenv IRAFBIN 	${iraf}bin$arch/

# Run the desired CL.
exec  ${IRAFBIN}$cl_binary
