# Bootstrap the UNIX bootstrap utilities and host system interface.
# Note - the environment variables HSI_CF and HSI_FF (compile/link flags)
# are required for the bootstrap; these are defined in hlib$irafuser.csh.
#
# USAGE:  `sh -x mkpkg.sh >& spool'  to bootstrap the IRAF HSI.

# Set `as' for the current host architecture.
#if test -h as; then rm -rf as; fi
#    ln -s as.$MACH as

# Ditto for `bin'.
#if test -h bin; then rm -rf bin; fi
#    ln -s bin.$MACH bin

echo "----------------------- OS -----------------------------"
echo "+"; echo "+"
(cd os;   sh -x mkpkg.sh)
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
