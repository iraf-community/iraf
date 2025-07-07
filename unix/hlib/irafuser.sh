#!/bin/bash
#
# IRAF definitions for the UNIX/bash user.  The $iraf path should be defined
# in the user's .login/.profile.


if [ ! -n "$iraf" ]; then
    echo "ERROR: \$iraf is not defined"
    exit 0
fi

if [ -e "$iraf/unix/hlib/util.sh" ]; then
    source $iraf/unix/hlib/util.sh
fi

if [ ! -n "$IRAFARCH" ]; then
    export IRAFARCH=`$iraf/unix/hlib/irafarch.sh`
    export MACH=$IRAFARCH
fi
if [ ! -n "$MACH" ]; then
    export MACH=`$iraf/unix/hlib/irafarch.sh`
else
    export MACH=$IRAFARCH
fi
            
export	hostid=unix
export	host=${iraf}/unix/
export	hlib=${iraf}/unix/hlib/
export	hbin=${iraf}/unix/bin.$MACH/
export	tmp=/tmp/

# Default to GCC for compilation.
export	CC=gcc
export	F77=$hlib/f77.sh
export	F2C=$hbin/f2c.e
export	RANLIB=ranlib

case "$MACH" in
  "macosx"|"macos64")
    export HSI_CF="-g -O2 -DSYSV -DMACOSX -DMACH64 -W -Wall -Wno-unused-parameter -arch arm64 -m64"
    export HSI_XF="-Inolibc -/DSYSV -/DMACOSX -/DMACH64 -/W -/Wall -/Wno-unused-parameter -/m64 -/arch -//arm64"
    export HSI_FF="-g -O2 -arch arm64 -m64 -DBLD_KERNEL"
    export HSI_LF="-arch arm64 -m64"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-z"
    ;;

  "macintel")
    export HSI_CF="-g -O2 -DSYSV -DMACOSX -DMACINTEL -DMACH64 -W -Wall -Wno-unused-parameter -arch x86_64 -m64 -mmacosx-version-min=10.14"
    export HSI_XF="-Inolibc -/DSYSV -/DMACOSX -/DMACINTEL -/DMACH64 -/W -/Wall -/Wno-unused-parameter -/arch -//x86_64 -/m64 -/mmacosx-version-min=10.14"
    export HSI_FF="-g -O2 -arch x86_64 -m64 -DMACH64 -DBLD_KERNEL -mmacosx-version-min=10.14"
    export HSI_LF="-arch x86_64 -m64 -DMACH64 -mmacosx-version-min=10.14"
    export HSI_F77LIBS=""
    export HSI_LFLAGS="-arch x86_64 -mmacosx-version-min=10.14"
    export HSI_OSLIBS=""
    #mkzflags="lflags=-z"
    ;;

  "linux64")
    export HSI_CF="-DLINUX -DREDHAT -DPOSIX -DSYSV -DLINUX64 -DMACH64 -W -Wall -Wno-unused-parameter -m64"
    export HSI_XF="-Inolibc -w -/m64 -/Wall -/W -/Wno-unused-parameter"
    export HSI_FF="-m64 -DBLD_KERNEL"
    export HSI_LF="-m64 "
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-Nxz -/Wl,-Bstatic"
    ;;

  "linux" | "redhat")
    export HSI_CF="-g -O2 -DLINUX -DREDHAT -DPOSIX -DSYSV -W -Wall -Wno-unused-parameter -m32"
    export HSI_XF="-Inolibc -/W -/Wall -/Wno-unused-parameter -/m32"
    export HSI_FF="-g -O2 -DBLD_KERNEL -m32"
    export HSI_LF="-Wl,-m,elf_i386 -m32"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-Nxz -/Wl,-Bstatic"
    ;;

*)
    echo 'Warning in hlib$irafuser.sh: unknown platform '"$MACH"
    exit 1
    ;;
esac


# Prepend a user <iraf.h> file to the compile flags in case we don't
# install as root.
#
export HSI_CF="-I${HOME}/.iraf/ $HSI_CF"
export HSI_FF="-I${HOME}/.iraf/ $HSI_FF"
export HSI_LF="-I${HOME}/.iraf/ $HSI_LF"
export HSI_XF="-I${HOME}/.iraf/ $HSI_XF"
export XC_CFLAGS="-I${HOME}/.iraf/"


# The following determines whether or not the VOS is used for filename mapping.
if [ -f ${iraf}lib/libsys.a ]; then
	export	HSI_LIBS="${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a ${hbin}libf2c.a -lm"
else
	export	HSI_CF="$HSI_CF -DNOVOS"
	export	HSI_LIBS="${hlib}libboot.a ${hlib}libos.a"
fi

export HSI_LIBS="$HSI_LIBS $HSI_OSLIBS"

# Useful host command aliases.
alias mkiraf=${hlib}mkiraf.sh
alias mkmlist=${hlib}mkmlist.sh
alias mkz="${hbin}mkpkg.e $mkzflags"

alias generic=${hlib}generic.sh
alias mkpkg=${hlib}mkpkg.sh
alias rmbin=${hlib}rmbin.sh
alias rmfiles=${hlib}rmfiles.sh
alias rtar=${hlib}rtar.sh
alias wtar=${hlib}wtar.sh
alias xc=${hlib}xc.sh
alias xyacc=${hlib}xyacc.sh
