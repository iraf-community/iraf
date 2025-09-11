#!/bin/csh
#
# IRAF definitions for the UNIX/csh user.  The $iraf path should be defined
# in the user's .login/.profile.


if ( ! "$?IRAFARCH" ) then
    setenv IRAFARCH `$iraf/unix/hlib/irafarch.csh`
    setenv MACH $IRAFARCH
endif
if ( ! "$?MACH" ) then
    setenv MACH `$iraf/unix/hlib/irafarch.csh`
else
    setenv MACH $IRAFARCH
endif


setenv	hostid	unix
setenv	host	${iraf}/unix/
setenv	hlib	${iraf}/unix/hlib/
setenv	hbin	${iraf}/unix/bin.$MACH/
setenv	tmp	/tmp/

# Default to GCC for compilation.
setenv	CC	gcc
setenv	F77	$hlib/f77.sh
setenv	F2C	$hbin/f2c.e
setenv	RANLIB	ranlib

switch ($MACH)
  case macosx:
  case macos64:
    setenv HSI_CF "-g -O2 -DSYSV -DMACOSX -DMACH64 -Wall -W -Wno-unused-parameter -arch arm64 -m64"
    if (-e /opt/homebrew/opt/openssl@3/include) then
        setenv HSI_CF "${HSI_CF} -DUSE_SSL -I/opt/homebrew/opt/openssl@3/include"
    endif
    setenv HSI_XF "-Inolibc -/DSYSV -/DMACOSX -/DMACH64 -/Wall -/W -W/no-unused-parameter-/m64 -/arch -//arm64"
    setenv HSI_FF "-g -O2 -arch arm64 -m64 -DBLD_KERNEL"
    setenv HSI_F77LIBS ""
    setenv HSI_LF "-arch arm64 -m64"
    setenv HSI_LFLAGS "-arch arm64 -m64"
    setenv HSI_OSLIBS ""
    setenv XC_CFLAGS  "-I${HOME}/.iraf/ -mmacosx-version-min=10.14"
    set    mkzflags = "'lflags=-z'"
    breaksw

  case macintel:
    setenv HSI_CF "-g -O2 -DSYSV -DMACOSX -DMACINTEL -DMACH64 -Wall -W -Wno-unused-parameter -arch x86_64 -m64 -mmacosx-version-min=10.14"
    if (-e /usr/local/opt/openssl@3/include) then
        setenv HSI_CF "${HSI_CF} -DUSE_SSL -I/usr/local/opt/openssl@3/include"
    endif
    setenv HSI_XF "-Inolibc -/DSYSV -/DMACOSX -/DMACINTEL -/DMACH64 -/Wall -/W -/Wno-unused-parameter -/arch -//x86_64 -/m64 -/mmacosx-version-min=10.14"
    setenv HSI_FF "-g -O2 -arch x86_64 -m64 -DMACH64 -DBLD_KERNEL -mmacosx-version-min=10.14"
    setenv HSI_LF "-arch x86_64 -m64 -DMACH64 -mmacosx-version-min=10.14"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS "-arch x86_64 -m64 -mmacosx-version-min=10.14"
    setenv HSI_OSLIBS ""
    setenv XC_CFLAGS  "-I${HOME}/.iraf/ -mmacosx-version-min=10.14"
    set    mkzflags = "'lflags=-z'"
    breaksw

  case linux64:
    setenv HSI_CF "-g -O2 -DLINUX -DREDHAT -DPOSIX -DSYSV -DLINUX64 -DMACH64 -DUSE_SSL -Wall -W -Wno-unused-parameter -m64"
    setenv HSI_XF "-Inolibc -/Wall -/W -/Wno-unused-parameter -/m64"
    setenv HSI_FF "-g -O2 -m64 -DBLD_KERNEL"
    setenv HSI_LF "-m64"
    setenv HSI_F77LIBS ""
    setenv HSI_LFLAGS ""
    setenv HSI_OSLIBS ""
    set    mkzflags = "'lflags=-Nxz -/Wl,-Bstatic'"
    breaksw

  case linux:
  case redhat:
    setenv HSI_CF "-O2 -DLINUX -DREDHAT -DPOSIX -DSYSV -Wall -W -Wno-unused-parameter -m32"
    setenv HSI_XF "-Inolibc -/Wall -/W -/Wno-unused-parameter -/m32"
    setenv HSI_FF "-O2 -DBLD_KERNEL -m32"
    setenv HSI_LF "-Wl,-m,elf_i386 -m32"
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

alias	generic	${hlib}generic.sh
alias	mkpkg	${hlib}mkpkg.sh
alias	rmbin	${hlib}rmbin.sh
alias	rmfiles	${hlib}rmfiles.sh
alias	rtar	${hlib}rtar.sh
alias	wtar	${hlib}wtar.sh
alias	xc	${hlib}xc.sh
alias	xyacc	${hlib}xyacc.sh
