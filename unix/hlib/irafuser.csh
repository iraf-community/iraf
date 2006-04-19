# IRAF definitions for the UNIX/csh user.  The additional variables iraf$ and
# home$ should be defined in the user's .login file.

setenv OS_MACH	`uname -s | tr '[A-Z]' '[a-z]' | cut -c1-6`
if (-f /etc/redhat-release) then
    if (`uname -m` == "ppc") then
	setenv MACH linuxppc
    else
	setenv MACH redhat
    endif
else if (-f /etc/SuSE-release) then
    if (`uname -m` == "ppc") then
	setenv MACH linuxppc
    else
	setenv MACH suse
    endif
else if (-f /etc/yellowdog-release || "`uname -m`" == "ppc") then
	setenv MACH linuxppc
else
    setenv MACH		`uname -s | tr '[A-Z]' '[a-z]'`
endif

if ($MACH == "darwin") then
    if ("`uname -m`" == "i386") then
        setenv MACH macintel
    else
        setenv MACH macosx
    endif
else if ($OS_MACH == "cygwin") then
    setenv MACH cygwin
endif

setenv	hostid	unix
setenv	host	${iraf}unix/
setenv	hlib	${iraf}unix/hlib/
setenv	hbin	${iraf}unix/bin.$MACH/
setenv	tmp	/tmp/

# Default to GCC for compilation.
setenv	CC	gcc
setenv	F77	$hlib/f77.sh
setenv	F2C	$hbin/f2c.e
setenv	RANLIB	ranlib

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

case macosx:
    setenv CC cc
    setenv CC_f2c cc
    setenv F2C $hbin/f2c.e

    setenv HSI_CF "-O -DMACOSX -w -Wunused"
    setenv HSI_XF "-Inolibc -/DMACOSX -w -/Wunused"
    if (`uname -r` == "5.5") then
	setenv HSI_CF 	"$HSI_CF -DOLD_MACOSX"
	setenv HSI_XF 	"$HSI_XF -DOLD_MACOSX"
    endif
    setenv HSI_FF "-O"
    setenv HSI_LF ""
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-z'"
    breaksw

case macintel:
    setenv CC cc
    #setenv F77	gcc
    setenv CC_f2c cc
    setenv F2C $hbin/f2c.e

    setenv HSI_CF "-O -DMACOSX -DMACINTEL -w -Wunused"
    setenv HSI_FF "-O"
    setenv HSI_LF ""
    setenv HSI_XF "-Inolibc -/DMACOSX -/DMACINTEL -w -/Wunused"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    if ($?IRAF_UNIBIN) then
        setenv HSI_CF "$HSI_CF -arch ppc -arch i386"
        setenv HSI_FF "$HSI_FF -arch ppc -arch i386"
        setenv HSI_LF "$HSI_LF"
        setenv HSI_LFLAGS "$HSI_LFLAGS -arch ppc -arch i386"
    endif
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-z'"
    breaksw

case linux:
    setenv HSI_CF "-O -DLINUX -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -w -/Wunused"
    setenv HSI_FF "-O"
    setenv HSI_LF ""
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

case redhat:
    setenv HSI_CF "-O -DLINUX -DREDHAT -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -w -/Wunused"
    setenv HSI_FF "-O"
    #setenv HSI_LF "-Wl,-Bstatic"
    setenv HSI_LF ""
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

case suse:
    setenv HSI_CF "-O -DSUSE -DLINUX -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -w -/Wunused"
    setenv HSI_FF "-O"
    #setenv HSI_LF "-Wl,-Bstatic -specs=/iraf/iraf//unix/bin.suse/gcc-specs"
    setenv HSI_LF "-specs=/iraf/iraf//unix/bin.suse/gcc-specs"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

case sunos:
    setenv HSI_CF "-O -DSOLARIS -DX86 -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -w -/Wunused"
    setenv HSI_FF "-O"
    #setenv HSI_LF "-t -Wl,-Bstatic"
    #setenv HSI_LFLAGS "-t -Wl,-Bstatic"
    #setenv HSI_OSLIBS \
    #	"-lsocket -lnsl -lintl -Wl,-Bdynamic -ldl -Wl,-Bstatic -lelf"
    setenv HSI_LF "-t"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS "-t"
    setenv HSI_OSLIBS "-lsocket -lnsl -lintl -ldl -lelf"
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

case linuxppc:
    setenv HSI_CF "-O -DLINUX -DREDHAT -DLINUXPPC -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -w -/Wunused"
    setenv HSI_FF "-O"
    setenv HSI_LF ""
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

case cygwin:
    setenv HSI_CF "-O -DCYGWIN -DLINUX -DREDHAT -DPOSIX -DSYSV -w -Wunused"
    setenv HSI_XF "-Inolibc -w -/Wunused -/DCYGWIN"
    setenv HSI_FF "-O"
    #setenv HSI_LF "-Wl,-Bstatic"
    setenv HSI_LF ""
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS "${iraf}unix/bin.cygwin/libcompat.a"
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

default:
    echo 'Warning in hlib$irafuser.csh: unknown platform '"$MACH"
    exit 1
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
