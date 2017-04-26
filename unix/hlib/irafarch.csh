#!/bin/csh -f
#
#  IRAFARCH -- Determine or set the current platform architecture parameters.
#
#  Usage:       irafarch
#		irafarch -set [<arch>] [opts]
#               irafarch [ -hsi | -nbits | -pipe | -tapecap | -tape ]
#
#	-mach		print the iraf architecture name [default]
#	-hsi		print the HSI arch
#	-nbits		print number of bits in an int (32 or 64)
#	-pipe		does platform support display fifo pipes?
#	-tapecap	does platform require tapecap changes?
#	-tape		does platform support tape drives?
#	-shlib		does platform support iraf shared libs?
#
#	-actual		print actual architecture name regardless of IRAFARCH
#	-set <arch>	manually reset the iraf environment architecture
#
# ----------------------------------------------------------------------------


unset	noclobber
onintr	cleanup_
unalias cd cp cmp echo ln mv rm sed set grep ls chmod chown pwd touch sort which

setenv	path  "(/sbin /usr/sbin /bin /usr/bin /usr/5bin /usr/ucb /etc /usr/etc $path /usr/local/bin /opt/local/bin /local/bin /home/local/bin /usr/openwin/bin /usr/X11R6/bin /usr/X11/bin)"

# set echo


##############################################################################
# START OF MACHDEP DEFINITIONS.
##############################################################################

set VERSION		= "V2.16.1"
set hmach 		= "INDEF"
set nbits 		= 32
set pipes 		= 1
set shlibs 		= 0
set tapecaps 		= 0
set tapes 		= 1

set debug 		= 0


#----------------------------------
# Determine platform architecture.
#----------------------------------

if (-e /usr/bin/uname) then
    set uname_cmd = /usr/bin/uname
else if (-e /bin/uname) then
    set uname_cmd = /bin/uname
else
    WARNING  "No 'uname' command found to determine architecture."
    exit 1
endif

setenv  UNAME	    `$uname_cmd    | tr '[A-Z]' '[a-z]'`
if ($UNAME == "sunos") then
    setenv  UNAME_M    `$uname_cmd -m | cut -c2- | tr '[A-Z]' '[a-z]'`
else
    setenv  UNAME_M    `$uname_cmd -m | tr '[A-Z]' '[a-z]' | tr ' ' '_'` 
endif
setenv  OSVERSION   `$uname_cmd -r | cut -c1`



# Allow an IRAFARCH definition in the environment to override.
if ($#argv == 1 && "$1" == "-actual") then
    setenv MNAME     $UNAME
    setenv MNAME_M   $UNAME_M
    unsetenv IRAFARCH

else if ($#argv == 1 && "$1" == "-current") then
    setenv MNAME     `/bin/ls -lad $iraf/bin | \
			awk '{ printf ("%s\n", $11) }' | \
			sed -e 's/bin.//g'`
    setenv MNAME_M   $UNAME_M
    setenv IRAFARCH  $MNAME
    goto repeat_

else
  if ($#argv == 0) then
    if ($?IRAFARCH) then
  repeat_:
	setenv MNAME     $IRAFARCH
	setenv MNAME_M   $UNAME_M
    else
        setenv MNAME     $UNAME
        setenv MNAME_M   $UNAME_M
    endif

  else
    if ($#argv != 0 && "$1" == "-set") then
        setenv MNAME     $2
        setenv MNAME_M   $2
    else
        setenv MNAME     $UNAME
        setenv MNAME_M   $UNAME_M
    endif
  endif
endif


# Set some common defaults for most platforms
set shlib		= 0			# no shared lib support
set nbits		= 32			# 32-bit architecture
set tapecaps 		= 1			# platform supports tapecaps
set tapes 		= 1			# platform support tape drives
set pipes 		= 1			# supports display fifo pipes
	
set pciraf		= 1			# PC-IRAF system
set suniraf		= 0			# SUN-IRAF system


if ($debug == 1) then				# DEBUG PRINT
    if ($?IRAFARCH) then
        echo " IRAFARCH = $IRAFARCH"
    endif
    echo "    MNAME = $MNAME"
    echo "  MNAME_M = $MNAME_M"
    echo "OSVERSION = $OSVERSION"
endif

# Determine parameters for each architecture.
switch ($MNAME) 
    case darwin: 					# Mac OS X
    case ipad:
    case macosx:
    case macintel:
        if ($?IRAFARCH) then
            set mach 		= "$IRAFARCH"
            set hmach 		= "$IRAFARCH"
	    if ("$mach" == "macintel") then
		set nbits	= 64
	    endif
	else 
            if ("$MNAME_M" == "x86_64") then		# 64-bit
                set mach 	= "macintel"
                set hmach 	= "macintel"
		set nbits	= 64
            else if ($MNAME_M == "x86" || $MNAME_M == "i386" || $MNAME_M == "ppc" || $MNAME_M == "power_macintosh") then
                set mach 	= "macosx"
                set hmach 	= "macosx"
		set nbits	= 32
            else
                set mach 	= "ipad"		# iOS Device
                set hmach 	= "ipad"
		set nbits	= 32
            endif
	endif
	set tapecaps 		= 0
	set tapes 		= 0
	set pipes 		= 0
        breaksw

    case redhat:
    case linux:
    case linux64:
        if ($?IRAFARCH) then
            set mach 		= "$IRAFARCH"
            set hmach 		= "$IRAFARCH"
	    if ("$mach" == "linux64") then
		set nbits	= 64
	    endif
	else 
            if ("$MNAME_M" == "x86_64") then		# Linux x86_64
                set mach 	= "linux64"
                set hmach 	= "linux64"
	        set nbits	= 64
            else					# Linux
                set mach 	= "linux"
                set hmach 	= "linux"
	        set nbits	= 32
            endif
        endif
        breaksw

    case ssun:
    case sparc:
    case sunos:
	set tapecaps 		= 1
        if ($UNAME_M != "86pc") then
	    set suniraf		= 1
	    set pciraf		= 0
            if ($OSVERSION == 5) then			# Sparc Solaris
                set mach  	= "ssun"
                set hmach 	= "ssol"
            else			   		# Sparc SunOS 4.x
                set mach  	= "sparc"
                set hmach 	= "sparc"
            endif
        else
            set mach 		= "sunos"	    	# Intel Solaris x86
            set hmach 		= "sunos"
	    set tapecaps 	= 0
	    set tapes 		= 0
	    set pipes 		= 0
        endif
        breaksw

    case freebsd: 					# FreeBSD
        set mach 		= "freebsd"
        set hmach 		= "freebsd"
	set tapecaps 		= 0
	set tapes 		= 0
	set pipes 		= 0
        breaksw

    default:      
	# We don't want to be limited by the CYGWIN version numbering so
	# look for a truncated match here before punting.
	set os_mach = `echo $UNAME | cut -c1-6`
	if ("$os_mach" == "cygwin") then
            set mach 		= "cygwin"
            set hmach 		= "cygwin"
	    set shlib		= 0
	    set tapecaps 	= 0
	    set tapes 		= 0
	    set pipes 		= 0
            breaksw

	else
	    echo  "Unable to configure platform IRAFARCH='$MNAME'."
	    exit 1
	endif
endsw

##############################################################################
# END OF MACHDEP DEFINITIONS.
##############################################################################


if ($#argv == 0) then
    echo $mach
else

    if ("$1" == "-mach") then
	echo $mach
    else if ("$1" == "-actual") then
	echo $mach
    else if ("$1" == "-current") then
	echo $mach
    else if ("$1" == "-hsi") then
	echo $hmach
    else if ("$1" == "-nbits") then
	echo $nbits
    else if ("$1" == "-pipes") then
	echo $pipes
    else if ("$1" == "-tapecap") then
	echo $tapecaps
    else if ("$1" == "-tapes") then
	echo $tapes
    else if ("$1" == "-shlib") then
	echo $shlib

    else if ("$1" == "-set") then
	if ("$2" != "") then
	    setenv IRAFARCH	   $2
	    shift ; shift
	endif
	goto repeat_
    else 
	echo "Invalid option '"$1"'"
    endif
endif

exit 0
