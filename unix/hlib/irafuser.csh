# IRAF definitions for the UNIX/csh user.  The additional variables iraf$ and
# home$ should be defined in the user's .login file.

if (-f /etc/redhat-release) then
    setenv MACH	redhat
else
    setenv MACH	`uname -s | tr '[A-Z]' '[a-z]'`
endif

setenv	hostid	unix
setenv	host	${iraf}unix/
setenv	hlib	${iraf}unix/hlib/
setenv	hbin	${iraf}unix/bin.$MACH/
setenv	tmp	/tmp/

switch ($MACH)
case freebsd:
    setenv HSI_CF "-O -DBSD -w -Wunused"
    setenv HSI_XF "-Inolibc -/DBSD -w -/Wunused"
    setenv HSI_FF "-O"
    setenv HSI_LF "-static"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS "-lcompat"
    set    mkzflags = "'lflags=-z' -/static"
    breaksw

case linux:
    setenv HSI_CF "-O -DLINUX -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -DLINUX -DPOSIX -DSYSV -w -/Wunused"
    setenv HSI_FF "-O"
    setenv HSI_LF "-Wl,-Bstatic"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

case redhat:
    setenv HSI_CF "-O -DLINUX -DREDHAT -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -DLINUX -DREDHAT -DPOSIX -DSYSV -w -/Wunused"
    setenv HSI_FF "-O"
    setenv HSI_LF ""
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

default:
    echo "Warning in hlib\$irafuser.csh: unknown platform `$MACH'"
    exit 1
    breaksw
endsw

# Setup to use GNU gcc/f2c for compilation.
setenv	CC	gcc
setenv	F77	$hlib/f77.sh
setenv	F2C	$hbin/f2c.e
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
alias	mkz	${hbin}mkpkg.e "$mkzflags"

alias	edsym	${hbin}edsym.e
alias	generic	${hbin}generic.e
alias	mkpkg	${hbin}mkpkg.e
alias	rmbin	${hbin}rmbin.e
alias	rmfiles	${hbin}rmfiles.e
alias	rtar	${hbin}rtar.e
alias	wtar	${hbin}wtar.e
alias	xc	${hbin}xc.e
alias	xyacc	${hbin}xyacc.e
