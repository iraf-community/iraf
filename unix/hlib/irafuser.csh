# IRAF definitions for the UNIX/csh user.  The additional variables iraf$ and
# home$ should be defined in the user's .login file.

setenv OSRELEASE `uname -r | cut -c1-3`
setenv OSVERSION `uname -r | cut -c1`

if ($OSVERSION == 5) then
	setenv MACH `uname -p`
	switch ($MACH)
	case sparc:
	    setenv MACH ssol
	    breaksw
	endsw

	set	HSI_FLAGS	= "-O -DSYSV -DSOLARIS -DSHLIB"
	set	HSI_XFLAGS	= "-Inolibc -/DSYSV -/DSOLARIS -/DSHLIB"
	set	HSI_LFLAGS	= "-t -Bstatic"
	setenv	HSI_OSLIBS "-lsocket -lnsl -lintl -Bdynamic -ldl -Bstatic -lelf"
	setenv	CC		cc
	setenv	F77		f77
	setenv	RANLIB		"echo ranlib"
else
	setenv	MACH		`mach`
	set	HSI_FLAGS	= "-O -DSUNOS -DSHLIB"
	set	HSI_XFLAGS	= "-Inolibc -/DSUNOS -/DSHLIB"
	setenv	HSI_LFLAGS	"-Bstatic"
	setenv	HSI_OSLIBS	""
	setenv	CC		cc
	setenv	F77		f77
	setenv	RANLIB		ranlib
endif

setenv	hostid	unix
setenv	host	${iraf}unix/
setenv	hlib	${iraf}unix/hlib/
setenv	hbin	${iraf}unix/bin.$MACH/
setenv	tmp	/tmp/

# Identify the C and Fortran compilers to be used.
#setenv	GCC_DEFS  "-DNOSTDHDRS -DSYSV -DANSI"
#setenv	GCC_WARN  "-W -Wunused -Wcomment"
#setenv	GCC_COMP  "-fstrength-reduce -fpcc-struct-return"
#setenv	HOS_DEFS  ""
#setenv	CC	"gcc $GCC_DEFS $HOS_DEFS $GCC_WARN $GCC_COMP"
#setenv	RANLIB	"echo ranlib"

# HSI cc, f77, and xc compile/link flags [MACHDEP].
switch ($MACH)
case ssol:
	setenv	HSI_CF	"$HSI_FLAGS"
	setenv	HSI_FF	"-O"
	setenv	HSI_XF	"$HSI_XFLAGS"
	setenv	HSI_LF	"$HSI_LFLAGS"
	setenv	HSI_F77LIBS ""
	breaksw
case i386:
case sparc:
	setenv  HSI_CF  "$HSI_FLAGS"
	setenv  HSI_FF  "-O"
	setenv	HSI_XF	"$HSI_XFLAGS"
	setenv  HSI_LF  "$HSI_LFLAGS"
	setenv  HSI_F77LIBS ""
	breaksw
case mc68020:
	setenv	HSI_CF	"-O -fsoft"
	setenv	HSI_FF	"-O -fsoft"
	setenv	HSI_XF	"-O -/fsoft -z"
	setenv	HSI_LF	"-fsoft"
	setenv	HSI_F77LIBS ""
	breaksw
default:
	setenv	HSI_CF	"-O"
	setenv	HSI_FF	"-O"
	setenv	HSI_XF	"-O"
	setenv	HSI_LF	""
	setenv	HSI_F77LIBS ""
	breaksw
endsw

# The following determines whether or not the VOS is used for filename mapping.
if (-f ${iraf}lib/libsys.a) then
	setenv	HSI_LIBS\
    "${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a"
else
	setenv	HSI_CF "$HSI_CF -DNOVOS"
	setenv	HSI_LIBS "${hlib}libboot.a ${hlib}libos.a"
endif

setenv HSI_LIBS "$HSI_LIBS $HSI_OSLIBS"

alias	mkiraf	${hlib}mkiraf.csh
alias	mkmlist	${hlib}mkmlist.csh
alias	mkz	${hbin}mkpkg.e "'lflags=-z -/Bstatic'"

alias	edsym	${hbin}edsym.e
alias	generic	${hbin}generic.e
alias	mkpkg	${hbin}mkpkg.e
alias	rmbin	${hbin}rmbin.e
alias	rmfiles	${hbin}rmfiles.e
alias	rtar	${hbin}rtar.e
alias	wtar	${hbin}wtar.e
alias	xc	${hbin}xc.e
alias	xyacc	${hbin}xyacc.e
