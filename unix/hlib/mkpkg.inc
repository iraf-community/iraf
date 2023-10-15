# Global (possibly system dependent) definitions for MKPKG.

$verbose

$set	MACH		= $(IRAFARCH)	# machine/fpu type
$set	HOSTID		= unix		# host system name
$set	SITEID		= noao		# site name

$ifeq (MACH, linux) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, linux64) then
$set	XFLAGS		= "-c -/O2"	# default XC compile flags
$set	XVFLAGS		= "-c -/O2"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"	# default XC link flags
$else $ifeq (MACH, macosx, macos64) then
$set	XFLAGS		= "-c -w -/O2"	# default XC compile flags
$set	XVFLAGS		= "-c -w -/O2"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, macintel) then
$set	XFLAGS		= "-c -w -/O2"	# default XC compile flags
$set	XVFLAGS		= "-c -w -/O2"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else
$set	XFLAGS		= "-c -w "	# default XC compile flags
$set	XVFLAGS		= "-c -w "	# VOPS XC compile flags
$set	LFLAGS		= "-z -/static"	# default XC link flags
$end

$set	USE_LIBMAIN	= yes		# update lib$libmain.o (root object)
$set	USE_KNET	= yes		# use the KI (network interface)
$set	USE_SHLIB	= no		# use (update) the shared library
$set	USE_CCOMPILER	= yes		# use the C compiler
$set	USE_GENERIC	= yes		# use the generic preprocessor
$set	USE_NSPP	= no		# make the NCAR/NSPP graphics kernel
$set    USE_IIS         = no		# make the IIS display control package
$set	USE_CALCOMP	= no		# make the Calcomp graphics kernel
$set	LIB_CALCOMP	= "-lcalcomp"	# name of host system calcomp library

$ifeq (MACH, macosx, macos64, macintel) then
    $include "hlib$mkpkg.sf.MACX"
$else $ifeq (MACH, linux) then
    $include "hlib$mkpkg.sf.LNUX"
$else $ifeq (MACH, linux64) then
    $include "hlib$mkpkg.sf.LNUX64"
$end
