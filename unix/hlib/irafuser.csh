# IRAF definitions for the UNIX/csh user.  The variable iraf$ should be
# defined in the user's .login file.

setenv	MACH		`uname -m`
set	HSI_FLAGS     = "-O -DPOSIX -DOSF1 -DSHLIB"
set	HSI_XFLAGS    = "-Inolibc -DPOSIX -DOSF1 -DSHLIB"
setenv	HSI_LFLAGS	"-taso"
setenv	HSI_OSLIBS	"-lots"
setenv	CC		cc
setenv	F77		f77
setenv	RANLIB		ranlib

# Get IRAF root directory.
if (! $?iraf) then
    foreach dir (/iraf/iraf /iraf /usr/iraf /opt/iraf/iraf /opt/iraf)
	if (-d $dir) then
	    setenv iraf ${dir}/
	    break
	endif
    end
endif

if (! $?iraf || ! -d $iraf) then
    echo "cannot find iraf root directory (\$iraf not defined)"
    exit 1
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
case alpha:
	setenv	HSI_CF	"$HSI_FLAGS"
	setenv	HSI_FF	"-w -O3"
	setenv	HSI_XF	"$HSI_XFLAGS"
	setenv	HSI_LF	"$HSI_LFLAGS"
	setenv	HSI_F77LIBS "-lUfor -lfor -lFutil -lm -lots"
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
alias	mkz	${hbin}mkpkg.e "'lflags=-z'"

alias	edsym	${hbin}edsym.e
alias	generic	${hbin}generic.e
alias	mkpkg	${hbin}mkpkg.e
alias	rmbin	${hbin}rmbin.e
alias	rmfiles	${hbin}rmfiles.e
alias	rtar	${hbin}rtar.e
alias	wtar	${hbin}wtar.e
alias	xc	${hbin}xc.e
alias	xyacc	${hbin}xyacc.e
