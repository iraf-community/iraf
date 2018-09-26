#!/bin/sh
# Bootstrap the UNIX bootstrap utilities and host system interface.
# Note - the environment variables HSI_CF and HSI_FF (compile/link flags)
# are required for the bootstrap; these are defined in hlib$irafuser.csh.
#
# USAGE:  `sh -x mkpkg.sh >& spool'  to bootstrap the IRAF HSI.

# Exit on error
set -e

# Set the HSI architecture.
sh -x setarch.sh

echo "----------------------- OS -----------------------------"
echo "+"; echo "+"
(cd os;   sh -x mkpkg.sh)
echo "----------------------- F2C ----------------------------"
echo "+"; echo "+"
(cd f2c; sh -x mkpkg.sh)
echo "----------------------- BOOT ---------------------------"
echo "+"; echo "+"
(cd boot; sh -x mkpkg.sh)
#echo "----------------------- SHLIB --------------------------"
#echo "+"; echo "+"
#(cd shlib; sh -x mkpkg.sh)
echo "----------------------- GDEV ---------------------------"
(cd gdev; sh -x mkpkg.sh)

# Install the newly created executables.
echo "install HSI executables in $host/bin.$MACH"
mv -f hlib/*.e bin.$MACH
