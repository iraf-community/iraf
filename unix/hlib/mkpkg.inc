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
$else $ifeq (MACH, ieee) then
$set	FLOAT		= "fi"
$set	XFLAGS		= "-c -q -/sa -/fi -/O2 -/na -/nw -/nv"
$set	XSFLAGS		= "-c -q -/sa -/fi -/O1 -/na -/nw -/nv"
$set	XVFLAGS		= "-c -q -/sa -/fi -/O2 -/na -/nw"
$set	LFLAGS		= "-z -/fi"
$else $ifeq (MACH, native) then
$set	FLOAT		= "fn"
$set	XFLAGS		= "-c -q -/sa -/fn -/O2 -/na -/nw -/nv"
$set	XSFLAGS		= "-c -q -/sa -/fn -/O1 -/na -/nw -/nv"
$set	XVFLAGS		= "-c -q -/sa -/fn -/O2 -/na -/nw"
$set	LFLAGS		= "-z -/fn"
$else
$set	XFLAGS		= "-c"
$set	XVFLAGS		= "-c"
$set	LFLAGS		= "-z"
$end

$set	USE_LIBMAIN	= yes		# update lib$libmain.o (root object)
$set	USE_KNET	= yes		# use the KI (network interface)
$set	USE_SHLIB	= no		# use (update) the shared library
$set	USE_CCOMPILER	= yes		# use the C compiler
$set	USE_GENERIC	= yes		# use the generic preprocessor
$set	USE_NSPP	= yes		# make the NCAR/NSPP graphics kernel
$set	USE_CALCOMP	= no		# make the Calcomp graphics kernel
$set	LIB_CALCOMP	= "-lcalcomp"	# name of host system calcomp library

$ifeq (MACH, sparc) then
$include "hlib$mkpkg.sf.SUN4"
$else $ifeq (MACH, i386) then
$include "hlib$mkpkg.sf.I386"
$else $ifeq (MACH, vax) then
$include "hlib$mkpkg.sf.VAX"
$else $ifeq (MACH, ieee) then
$include "hlib$mkpkg.sf.CX"
$else $ifeq (MACH, native) then
$include "hlib$mkpkg.sf.CX"
$end
