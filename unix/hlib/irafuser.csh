# IRAF definitions for the UNIX/csh user.  The additional variables iraf$ and
# home$ should be defined in the user's .login file.

setenv	MACH		linux

setenv	hostid	unix
setenv	host	${iraf}unix/
setenv	hlib	${iraf}unix/hlib/
setenv	hbin	${iraf}unix/bin.$MACH/
setenv	tmp	/tmp/

setenv	HSI_CF	"-O -DSYSV -DLINUX -DSOLARIS -w -b i486-linuxaout -Wunused -traditional"
setenv	HSI_XF	"-/DSYSV -/DLINUX -/DSOLARIS -w -/Wunused -/traditional"
setenv	HSI_FF	"-O -b i486-linuxaout"
setenv	HSI_LF	"-b i486-linuxaout -static"

setenv	HSI_F77LIBS	""
setenv	HSI_LFLAGS	""
setenv	HSI_OSLIBS	""

setenv	CC	gcc
setenv	F77	$hlib/f77.sh
setenv	RANLIB	ranlib

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
alias	mkz	${hbin}mkpkg.e "'lflags=-z' -/static"

alias	edsym	${hbin}edsym.e
alias	generic	${hbin}generic.e
alias	mkpkg	${hbin}mkpkg.e
alias	rmbin	${hbin}rmbin.e
alias	rmfiles	${hbin}rmfiles.e
alias	rtar	${hbin}rtar.e
alias	wtar	${hbin}wtar.e
alias	xc	${hbin}xc.e
alias	xyacc	${hbin}xyacc.e
