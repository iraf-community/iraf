# Global (possibly system dependent) definitions for MKPKG.

$verbose

$set	MACH		= $(IRAFARCH)	# machine/fpu type
$set	HOSTID		= unix		# host system name
$set	SITEID		= noao		# site name

$ifeq (MACH, ddec) then
$set	XFLAGS		= "-c -/G -//0"
$set	XVFLAGS		= "-c -/G -//0"
$set	LFLAGS		= ""
$else $ifeq (MACH, dmip) then
$set	XFLAGS		= "-c -/G -//0 -/Olimit -//1024"
$set	XVFLAGS		= "-c -/G -//0 -/Olimit -//1024"
$set	LFLAGS		= ""
$else
$set	XFLAGS		= "-c -/G -//0"
$set	XVFLAGS		= "-c -/G -//0"
$set	LFLAGS		= ""
$end

$set	USE_LIBMAIN	= yes		# update lib$libmain.o (root object)
$set	USE_KNET	= yes		# use the KI (network interface)
$set	USE_SHLIB	= no		# use (update) the shared library
$set	USE_CCOMPILER	= yes		# use the C compiler
$set	USE_GENERIC	= yes		# use the generic preprocessor
$set	USE_NSPP	= yes		# make the NCAR/NSPP graphics kernel
$set    USE_IIS         = yes           # make the IIS display control package
$set	USE_CALCOMP	= no		# make the Calcomp graphics kernel
$set	LIB_CALCOMP	= "-lcalcomp"	# name of host system calcomp library

$ifeq (MACH, ddec) then
$include "hlib$mkpkg.sf.DDEC"
$else $ifeq (MACH, dmip) then
$include "hlib$mkpkg.sf.DMIP"
$end
