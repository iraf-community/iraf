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
unalias cd cp echo sed set grep ls

setenv	path  "(/sbin /usr/sbin /bin /usr/bin /usr/5bin /usr/ucb /etc /usr/etc $path /usr/local/bin /opt/local/bin /local/bin /home/local/bin /usr/openwin/bin /usr/X11R6/bin /usr/X11/bin)"

# set echo

# Check for bogus IRAFARCH value.
if ($?IRAFARCH) then
    if ( "$IRAFARCH" == "") then
        unsetenv IRAFARCH
    endif
endif


##############################################################################
# START OF MACHDEP DEFINITIONS.
##############################################################################

set VERSION		= "V2.18.1"
set hmach 		= "INDEF"
set nbits 		= 64
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
setenv  UNAME_M    `$uname_cmd -m | tr '[A-Z]' '[a-z]' | tr ' ' '_'` 
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
set nbits		= 64			# 64-bit arch is default
set tapecaps 		= 1			# platform supports tapecaps
set tapes 		= 1			# platform support tape drives
set pipes 		= 1			# supports display fifo pipes
	

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
    case macosx: 					# Mac OS X
    case macos64: 					# Mac (alternate)
    case macintel:
    case darwin:
        if ($?IRAFARCH) then
            set mach 		= "$IRAFARCH"
            set hmach 		= "$IRAFARCH"
	    set nbits	        = 64    # All Mac systems are now 64-bit only
	else 
            if ("$MNAME_M" == "x86_64") then		# 64-bit Intel
                set mach 	= "macintel"
                set hmach 	= "macintel"
		set nbits	= 64
            else if ($MNAME_M == "arm64") then          # Apple M1/M2
                set mach 	= "macosx"
                set hmach 	= "macosx"
		set nbits	= 64
            endif
	endif
	set tapecaps 		= 0                     # No tape support
	set tapes 		= 0                     # No tape support
	set pipes 		= 1                     # Display pipes
        breaksw

    case redhat:
    case linux:
    case linux64:
        if ($?IRAFARCH) then
            set mach 		= "$IRAFARCH"
            set hmach 		= "$IRAFARCH"
	    if ("$mach" == "linux64") then
		set nbits	= 64
	    else
		set nbits	= 32
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

    default:      
	echo  "Unable to configure platform IRAFARCH='$MNAME'."
	exit 1
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
