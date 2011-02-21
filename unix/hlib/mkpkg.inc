# Global (possibly system dependent) definitions for MKPKG.

$verbose

$set	MACH		= $(IRAFARCH)	# machine/fpu type
$set	HOSTID		= unix		# host system name
$set	SITEID		= noao		# site name

$ifeq (MACH, freebsd) then
$set	XFLAGS		= "-c -w -/m32"	# default XC compile flags
$set	XVFLAGS		= "-c -w" -/m32	# VOPS XC compile flags
$set	LFLAGS		= "-z -/static -/m32"	# default XC link flags
$else $ifeq (MACH, linux) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, linux64) then
$set	XFLAGS		= "-c -w -/g -/m64"	# default XC compile flags
$set	XVFLAGS		= "-c -w -/g -/m64"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz   -/g -/m64"	# default XC link flags
$else $ifeq (MACH, redhat) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, macosx) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, macintel) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, ipad) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, linuxppc) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, sunos) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else $ifeq (MACH, cygwin) then
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags
$else
$set	XFLAGS		= "-c -w"	# default XC compile flags
$set	XVFLAGS		= "-c -w"	# VOPS XC compile flags
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

$ifeq (MACH, linux, redhat, macosx, macintel, ipad) then
    $include "hlib$mkpkg.sf.MACX"
$else $ifeq (MACH, linux, redhat, linuxppc) then
    $include "hlib$mkpkg.sf.LNUX"
$else $ifeq (MACH, linux64) then
    $include "hlib$mkpkg.sf.LNUX64"
$else $ifeq (MACH, freebsd) then
    $include "hlib$mkpkg.sf.FBSD"
$else $ifeq (MACH, sunos) then
    $include "hlib$mkpkg.sf.SX86"
$else $ifeq (MACH, cygwin) then
    $include "hlib$mkpkg.sf.CYGW"
$end
