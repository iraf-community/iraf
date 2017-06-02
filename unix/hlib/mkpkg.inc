# Global (possibly system dependent) definitions for MKPKG.

$verbose

$set	MACH		= $(IRAFARCH)	# machine/fpu type
$set	HOSTID		= unix		# host system name
$set	SITEID		= noao		# site name

$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"	        # default XC link flags

$set	USE_LIBMAIN	= yes		# update lib$libmain.o (root object)
$set	USE_KNET	= yes		# use the KI (network interface)
$set	USE_SHLIB	= no		# use (update) the shared library
$set	USE_CCOMPILER	= yes		# use the C compiler
$set	USE_GENERIC	= yes		# use the generic preprocessor
$set	USE_NSPP	= no		# make the NCAR/NSPP graphics kernel
$set    USE_IIS         = no		# make the IIS display control package
$set	USE_CALCOMP	= no		# make the Calcomp graphics kernel
$set	LIB_CALCOMP	= "-lcalcomp"	# name of host system calcomp library

$include "hlib$mkpkg.sf"
