#!/bin/csh -f
#
# IRAF definitions for the UNIX/csh user.  The additional variables iraf$ and
# home$ should be defined in the user's .login file.


set old_method		= 0

if ($old_method == 1) then

setenv OS_MACH	`uname -s | tr '[A-Z]' '[a-z]' | cut -c1-6`

if (`uname -m` == "x86_64") then
    if ($OS_MACH == "darwin") then
        setenv MACH darwin
        setenv IRAFARCH darwin
    else
        setenv MACH linux64
        setenv IRAFARCH linux64
    endif
else if (-f /etc/redhat-release) then
    setenv MACH redhat
else
    setenv MACH		`uname -s | tr '[A-Z]' '[a-z]'`
endif

if ($MACH == "darwin") then
    # Let the IRAFARCH override the machine to support cross compilation.
    if ($?IRAFARCH) then
        if ("$IRAFARCH" == "macosx") then
	    setenv MACH macosx
        else if ("$IRAFARCH" == "macintel") then
	    setenv MACH macintel
        endif
    else
        if ("`uname -m`" == "i386") then
            setenv MACH macosx
            setenv IRAFARCH macosx
        else if ("`uname -m`" == "x86_64") then
            setenv MACH macintel
            setenv IRAFARCH macintel
        else 
            setenv MACH ipad
            setenv IRAFARCH ipad
        endif
    endif
else if ($OS_MACH == "cygwin") then
    setenv MACH cygwin
endif

else		# old_method
            
    set a = `$iraf/unix/hlib/irafarch.csh`
    if ($status == 0) then
        setenv MACH 	 $a
        setenv IRAFARCH  $a
    else
	echo "Error:  "$a
	exit 1
    endif
            
endif		# old_method



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
    setenv HSI_CF "-O -DBSD -DPOSIX -w -Wunused -m32"
    setenv HSI_XF "-Inolibc -/DBSD -w -/Wunused -/m32"
    setenv HSI_FF "-O -DBLD_KERNEL -m32"
    setenv HSI_LF "-static -m32 -B/usr/lib32 -L/usr/lib32"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS "-lcompat"
    set    mkzflags = "'lflags=-z' -/static"
    breaksw

case macosx:
    setenv HSI_CF "-O -DMACOSX -w -Wunused -m32"
    setenv HSI_XF "-Inolibc -/DMACOSX -w -/Wunused -/m32"
    setenv HSI_FF "-O -m32 -DBLD_KERNEL"
    setenv HSI_LF "-m32"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    setenv MACOSX_DEPLOYMENT_TARGET "10.5"
    set    mkzflags = "'lflags=-z'"
    breaksw

case macintel:
    setenv HSI_CF "-O -DMACOSX -DMACINTEL -DMACH64 -w -Wunused -m64 -g"
    setenv HSI_XF "-Inolibc -/DMACOSX -/DMACINTEL -w -/Wunused -/DMACH64 -/m64"
    setenv HSI_FF "-O -m64 -DMACH64 -DBLD_KERNEL"
    setenv HSI_LF "-m64 -DMACH64"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    setenv MACOSX_DEPLOYMENT_TARGET "10.5"
    set    mkzflags = "'lflags=-z'"
    breaksw

case ipad:
    setenv XC_CFLAGS	"-I/var/include"
    setenv HSI_CF "-O -I/var/include -DMACOSX -DMACINTEL -DIPAD -w -Wunused"
    setenv HSI_XF "-Inolibc -/DMACOSX -/DMACINTEL -/DIPAD -w -/Wunused"
    setenv HSI_FF "-O -DBLD_KERNEL"
    setenv HSI_LF ""
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-z'"
    breaksw

case linux64:
    setenv HSI_CF "-g -DLINUX -DREDHAT -DPOSIX -DSYSV -DLINUX64 -DMACH64 -w -m64"
    setenv HSI_XF "-g -Inolibc -w -/m64 -/Wunused"
    setenv HSI_FF "-g -m64 -DBLD_KERNEL"
    setenv HSI_LF "-m64 "
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

case linux:
case redhat:
    setenv HSI_CF "-O -DLINUX -DREDHAT -DPOSIX -DSYSV -w -m32 -Wunused"
    setenv HSI_XF "-Inolibc -w -/Wunused -/m32"
    setenv HSI_FF "-O -DBLD_KERNEL -m32"
    setenv HSI_LF "-m32"
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


# Prepend a user <iraf.h> file to the compile flags in case we don't
# install as root.
#
setenv HSI_CF  	  "-I${iraf}include $HSI_CF"
setenv HSI_FF  	  "-I${iraf}include $HSI_FF"
setenv HSI_LF  	  "-I${iraf}include $HSI_LF"
setenv HSI_XF  	  "-I${iraf}include $HSI_XF"
setenv XC_CFLAGS  "-I${iraf}include"


# The following determines whether or not the VOS is used for filename mapping.
if (-f ${iraf}lib/libsys.a) then
	setenv	HSI_LIBS\
    "${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a ${hbin}libf2c.a -lm"
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
