# Global MKPKG definitions for the LOCAL package.

$set XFLAGS	= "$(XFLAGS) -p local"
$set XVFLAGS	= "$(XVFLAGS) -p local"
$set LFLAGS	= "$(LFLAGS) -p local"

# Uncomment and modify the following to add special file list entries for
# various machine architectures and Fortran compilers.

# $ifeq (MACH, sparc) then
# $include "local$lib/mkpkg.sf.sun4"
# $else $ifeq (MACH, vms) then
# $include "local$lib/mkpkg.sf.vms"
# $end
