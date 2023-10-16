#!/bin/csh -f
#
# IRAF definitions for the UNIX/csh user.  The $iraf path should be defined
# in the user's .login/.profile.


set a = `$iraf/unix/hlib/irafarch.csh`
if ($status == 0) then
    setenv MACH 	 $a
    setenv IRAFARCH  $a
else
    echo "Error:  "$a
    exit 1
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
  case macosx:
  case macos64:
    setenv HSI_CF "-g -O2 -DSYSV -DMACOSX -DMACH64 -Wall -arch arm64 -m64"
    setenv HSI_XF "-Inolibc -/DSYSV -/DMACOSX -/DMACH64 -/Wall -/m64 -/arch -//arm64"
    setenv HSI_FF "-g -O2 -arch arm64 -m64 -DBLD_KERNEL"
    setenv HSI_LF "-arch arm64 -m64"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-z'"
    breaksw

  case macintel:
    setenv HSI_CF "-g -O2 -DSYSV -DMACOSX -DMACINTEL -DMACH64 -Wall -m64"
    setenv HSI_XF "-Inolibc -/DSYSV -/DMACOSX -/DMACINTEL -/DMACH64 -/Wall -/m64"
    setenv HSI_FF "-g -O2 -m64 -DMACH64 -DBLD_KERNEL"
    setenv HSI_LF "-m64 -DMACH64"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-z'"
    breaksw

  case linux64:
    setenv HSI_CF "-g -O2 -DLINUX -DREDHAT -DPOSIX -DSYSV -DLINUX64 -DMACH64 -Wall -m64"
    setenv HSI_XF "-Inolibc -/Wall -/m64 -/Wunused"
    setenv HSI_FF "-g -O2 -m64 -DBLD_KERNEL"
    setenv HSI_LF "-m64 "
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

  case linux:
  case redhat:
    setenv HSI_CF "-O2 -DLINUX -DREDHAT -DPOSIX -DSYSV -Wall -m32 -Wunused"
    setenv HSI_XF "-Inolibc -/Wunused -/m32"
    setenv HSI_FF "-O2 -DBLD_KERNEL -m32"
    setenv HSI_LF "-m32"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
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
setenv HSI_CF  	  "-I${HOME}/.iraf/ $HSI_CF"
setenv HSI_FF  	  "-I${HOME}/.iraf/ $HSI_FF"
setenv HSI_LF  	  "-I${HOME}/.iraf/ $HSI_LF"
setenv HSI_XF  	  "-I${HOME}/.iraf/ $HSI_XF"
setenv XC_CFLAGS  "-I${HOME}/.iraf/"


# The following determines whether or not the VOS is used for filename mapping.
if (-f ${iraf}lib/libsys.a) then
	setenv	HSI_LIBS\
    "${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a ${hbin}libf2c.a -lm"
else
	setenv	HSI_CF "$HSI_CF -DNOVOS"
	setenv	HSI_LIBS "${hlib}libboot.a ${hlib}libos.a"
endif

setenv HSI_LIBS "$HSI_LIBS $HSI_OSLIBS"

# Useful host command aliases
alias	mkiraf	${hlib}mkiraf.sh
alias	mkmlist	${hlib}mkmlist.sh
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
