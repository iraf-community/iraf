#!/bin/csh -f
#
# IRAF definitions for the UNIX/csh user.  The additional variables iraf$ and
# home$ should be defined in the user's .login file.


setenv MACH 	 `${iraf}unix/hlib/irafarch.sh`
setenv IRAFARCH  `${iraf}unix/hlib/irafarch.sh`



setenv	hostid	unix
setenv	host	${iraf}unix/
setenv	hlib	${iraf}unix/hlib/
setenv	hbin	${iraf}unix/bin.$MACH/
setenv	tmp	/tmp/

# Default to GCC for compilation.
setenv	CC	gcc
setenv	F77	${hlib}f77.sh
setenv	F2C	${hbin}f2c.e
setenv	RANLIB	ranlib
if ( ! ${?CPPFLAGS} ) then
    set CPPFLAGS
endif
if (! ${?CFLAGS} ) then
    set CFLAGS
endif
if (! ${?LDFLAGS} ) then
    set LDFLAGS
endif

setenv XC_CFLAGS "${CPPFLAGS} ${CFLAGS} -I${iraf}include"
setenv HSI_CF "${XC_CFLAGS}"
setenv HSI_XF "-x -Inolibc -/Wall -/O2"
setenv HSI_FF "-g -DBLD_KERNEL -O2"
setenv HSI_LF "${LDFLAGS}"
setenv HSI_F77LIBS ""
setenv HSI_LFLAGS ""
setenv HSI_OSLIBS ""

# The following determines whether or not the VOS is used for filename mapping.
if (-f ${iraf}lib/libsys.a) then
	setenv	HSI_LIBS "${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a ${hbin}libf2c.a -lm"
else
	setenv	HSI_CF "$HSI_CF -DNOVOS"
	setenv	HSI_LIBS "${hlib}libboot.a ${hlib}libos.a -lm"
endif

setenv HSI_LIBS "$HSI_LIBS $HSI_OSLIBS"

alias	mkiraf	${hlib}mkiraf.sh
alias	mkmlist	${hlib}mkmlist.sh

alias	edsym	${hbin}edsym.e
alias	generic	${hbin}generic.e
alias	mkpkg	${hbin}mkpkg.e
alias	rmbin	${hbin}rmbin.e
alias	rmfiles	${hbin}rmfiles.e
alias	rtar	${hbin}rtar.e
alias	wtar	${hbin}wtar.e
alias	xc	${hbin}xc.e
alias	xyacc	${hbin}xyacc.e
