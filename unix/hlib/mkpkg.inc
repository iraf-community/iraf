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
$else $ifeq (MACH, m68k_fpa) then
$set	XFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,FPA1"	# default XC
$set	XVFLAGS		= "-cq -/W0,-opt,3,-db,-cpu,FPA1"	# XC VOPS flags
$set	XSFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,FPA1"	# XC libsys 
$set	XEFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,FPA1"	# XC libex
$set	LFLAGS		= "-z -/w -/W0,-db"
$else $ifeq (MACH, m68k_f68) then
$set	XFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,3000"	# default XC
$set	XVFLAGS		= "-cq -/W0,-opt,3,-db,-cpu,3000"	# XC VOPS flags
$set	XSFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,3000"	# XC libsys 
$set	XEFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,3000"	# XC libex
$set	LFLAGS		= "-z -/w -/W0,-db"
$else $ifeq (MACH, a88k) then
$set	XFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,a88k -/DA88K"
$set	XVFLAGS		= "-cq -/W0,-opt,3,-db,-cpu,a88k"	# XCVOPS flags
$set	XSFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,a88k"	# XC libsys 
$set	XEFLAGS		= "-cq -/W0,-opt,2,-db,-cpu,a88k"	# XC libex
$set	LFLAGS		= "-z -/W0,-dbs"
$else
$set	XFLAGS		= "-c -/$(MACH)"
$set	XVFLAGS		= "-c -/$(MACH)"
$set	LFLAGS		= "-/Bstatic -/$(MACH)"
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
$else $ifeq (MACH, m68k_fpa) then
$include "hlib$mkpkg.sf.M68K"
$else $ifeq (MACH, m68k_f68) then
$include "hlib$mkpkg.sf.M68K"
$else $ifeq (MACH, a88k) then
$include "hlib$mkpkg.sf.A88K"
$else
$include "hlib$mkpkg.sf.SUN3"
$end
