#!/bin/sh
# cl.sh -- Startup the version of the CL executable compiled for the
# architecture or floating point hardware appropriate for the current
# machine.

PREFIX=

iraf="$PREFIX/iraf/iraf/"
export iraf

host="${iraf}unix/"
hconfig="${host}config/"
hscripts="${host}scripts/"

# Determine CL binary to run based on how we were called.

cl_binary="cl.e"

# Check for a version query.
if [ "$1" != "" ]; then
    if [ "$1" = "-v" -o "$1" = "-version" -o \
	 "$1" = "-V" -o "$1" = "--version" ]; then
	head -1 ${hconfig}motd
	exit 0
    fi
fi

# Determine platform architecture.
ARG_MACH=auto
. ${hscripts}setup.sh

# Recent linux systems display a problem in how pointer addresses 
# interact with the stack and can result in a segfault.  Remove the
# stacksize limit for IRAF processes until this is better understood.
if [ "$OPERATING_SYSTEM" = "linux" ]; then
    ulimit -s unlimited
fi

IRAFBIN=${iraf}bin/
export IRAFBIN
file=${IRAFBIN}$cl_binary
if [ -x $file ]; then
    exec $file $@
else
    echo "$file not found"
fi
