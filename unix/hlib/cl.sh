#!/bin/sh
# sh version of cl.csh - use this in the unified release
#
# This is much shorter than the csh script that IRAF distributes
# because we don't dance around the IRAFARCH issue -- we just use
# IRAFARCH.  That means that all we need to do is recognize the
# cl parameters that are not implemented by cl.e / ecl.e

binary=ecl.e

case "$1"
in
-v|-version|-V|--version)
	head -1 $iraf/unix/hlib/motd
	exit 0
	;;
-old|-o)
	binary=cl.e
	;;
esac


case "$IRAFARCH"
in
redhat)		ulimit -s unlimited ;;
linux)		ulimit -s unlimited ;;
linux64)	ulimit -s unlimited ;;
'')		echo IRAFARCH not set ; exit 1 ;;
esac

# cl is goofy to need all this crap:

arch=.$IRAFARCH
IRAFBIN=$iraf/bin.$IRAFARCH/

export arch IRAFBIN

exec $iraf/bin.$IRAFARCH/$binary

# JT: Should we also set "mach" and "MACH", like the original cl.csh?
#
# The new cl.csh also uses a new script hlib/irafarch.csh to tell it the
# architecture, which in principle we could also now use to set IRAFARCH.

