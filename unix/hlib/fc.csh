#!/bin/csh
# FC.CSH -- Link an IMFORT or host fortran program from IRAF.  A front end
# to XC, the purpose of this script is to determine the IRAF architecture
# and add the appropriate host compiler file to XC.

# set echo

# Determine platform architecture.
setenv	MACH	`uname -m`

set need_outfile = 1
set outfile = ""

# Scan the argument list and concatenate all arguments.
set args = ""
while ("$1" != "")
    if ($need_outfile && $1:e == "f" || $1:e == "o") then
	set outfile = $1:r".e"
	set need_outfile = 0
    else if ("$1" == "-o") then
	set need_outfile = 0
	set outfile = ""
    endif
    set args = "$args $1"
    shift
end

if ($outfile != "") then
    set out = "-o $outfile"
else
    set out = ""
endif

# Determine the desired architecture.
if (! $?IRAFARCH) then
    setenv IRAFARCH "alpha"
endif

# Get float option switch.
switch ($IRAFARCH)
case alpha:
    set float = "/usr/lib/cmplrs/fort/for_main.o"
    breaksw
default:
    set float = ""
    breaksw
endsw

# Call XC with the appropriate float option.
xc $out $float $args
