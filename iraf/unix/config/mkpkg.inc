# Global (possibly system dependent) definitions for MKPKG.

$verbose

$set	MACH		= $(IRAFARCH)	# machine/fpu type
$set	HOSTID		= unix		# host system name
$set	SITEID		= noao		# site name

$set	XFLAGS		= "-c"		# default XC compile flags
$set	XVFLAGS		= "-c"		# VOPS XC compile flags
$set	LFLAGS		= "-Nz"		# default XC link flags

$set	USE_LIBMAIN	= yes		# update lib$libmain.o (root object)
$set	USE_KNET	= yes		# use the KI (network interface)
$set	USE_SHLIB	= no		# use (update) the shared library
$set	USE_CCOMPILER	= yes		# use the C compiler
$set	USE_GENERIC	= yes		# use the generic preprocessor
$set	USE_NSPP	= no		# make the NCAR/NSPP graphics kernel
$set	USE_IIS         = no		# make the IIS display control package
$set	USE_CALCOMP	= no		# make the Calcomp graphics kernel
$set	LIB_CALCOMP	= "-lcalcomp"	# name of host system calcomp library

$special "sys$osb/":		aclrb.c		as$aclrb.c
				bytmov.c	as$bytmov.c
				ieeer.x		as$ieeer.x
				ieeed.x		as$ieeed.x
				;

$special "sys$vops/ak/":	aclrc.x		as$aclrc.c
				aclrs.x		as$aclrs.c
				aclri.x		as$aclri.c
				aclrl.x		as$aclrl.c
				aclrr.x		as$aclrr.c
				aclrd.x		as$aclrd.c
				;

$special "sys$vops/lz/":	amovc.x		as$amovc.c
				amovs.x		as$amovs.c
				amovi.x		as$amovi.c
				amovl.x		as$amovl.c
				amovr.x		as$amovr.c
				amovd.x		as$amovd.c
				;

$set	XBIG	= '& "$xc -c -w -/Nx512 -/Ns3072 &"'
$special "sys$fmtio/":		evvexpr.x	$(XBIG)	;

$set    XNL     = '& "$xc -c -/NL400 &"'
$special "math$slalib/":        obs.f           $(XNL)  ;

