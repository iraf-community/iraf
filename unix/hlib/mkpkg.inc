# Global (possibly system dependent) definitions for MKPKG.

$verbose

$set	MACH		= $(IRAFARCH)	# machine/fpu type
$set	HOSTID		= unix		# host system name
$set	SITEID		= noao		# site name

$ifeq (MACH, sparc) then
$set	XFLAGS		= "-c"		# default XC compile flags
$set	XVFLAGS		= "-c"		# VOPS XC compile flags
$set	LFLAGS		= "-/Bstatic"	# default XC link flags
$else $ifeq (MACH, i386) then
$set	XFLAGS		= "-c"
$set	XVFLAGS		= "-c"
$set	LFLAGS		= "-/Bstatic"
$else $ifeq (MACH, pg) then
$set	XFLAGS		= "-c -/$(MACH) -/f68881"
$set	XVFLAGS		= "-c -/$(MACH) -/f68881"
$set	LFLAGS		= "-/Bstatic -/$(MACH) -/f68881"
$set	MACH		= f68881
$else
$set	XFLAGS		= "-c -/$(MACH)"
$set	XVFLAGS		= "-c -/$(MACH)"
$set	LFLAGS		= "-/Bstatic -/$(MACH)"
$end

$set	USE_LIBMAIN	= yes		# update lib$libmain.o (root object)
$set	USE_KNET	= yes		# use the KI (network interface)
$set	USE_SHLIB	= yes		# use (update) the shared library
$set	USE_CCOMPILER	= yes		# use the C compiler
$set	USE_GENERIC	= yes		# use the generic preprocessor
$set	USE_NSPP	= yes		# make the NCAR/NSPP graphics kernel
$set	USE_CALCOMP	= no		# make the Calcomp graphics kernel
$set	LIB_CALCOMP	= "-lcalcomp"	# name of host system calcomp library

$ifeq (MACH, sparc) then
$include "hlib$mkpkg.sf.SUN4"
$else $ifeq (MACH, i386) then
$include "hlib$mkpkg.sf.I386"
$else
$include "hlib$mkpkg.sf.SUN3"
$end
