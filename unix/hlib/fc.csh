#!/bin/csh
# FC.CSH -- Link an IMFORT or host fortran program from IRAF.  A front end
# to XC, the purpose of this script is to determine the IRAF architecture
# and add the appropriate host compiler file to XC.

# set	echo

if ("$ISP" == "") then
    set MACH = "m68k"
else
    set	MACH = `echo $ISP`	# Domain/OS specific (m68k or a88k arch.)
endif
#set	MACH = `mach`		# SUNOS specific.
#set	MACH = convex		# other systems
set	args = ""

# Scan the argument list and concatenate all arguments.
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
    else if ("$MACH" == "sparc") then
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
case m68k_fpa:
    set float = "-/W0,-cpu,FPA1"
    breaksw
case m68k_f68:
    set float = "-/W0,-cpu,3000"
    breaksw
case a88k:
    set float = "-/W0,-cpu,a88k"
    breaksw
default:
    set float = ""
    breaksw
endsw

# Call XC with the appropriate float option.
xc $float $args
