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

export MACH=`$iraf/unix/hlib/irafarch.sh`
export IRAFARCH=`$iraf/unix/hlib/irafarch.sh`
            

export	hostid=unix
export	host=${iraf}unix/
export	hlib=${iraf}unix/hlib/
export	hbin=${iraf}unix/bin.$MACH/
export	tmp=/tmp/

# Default to GCC for compilation.
export	CC=gcc
export	F77=$hlib/f77.sh
export	F2C=$hbin/f2c.e
export	RANLIB=ranlib

case "$MACH" in
  "macosx"|"macos64")
    export HSI_CF="-O -DSYSV -DMACOSX -DMACH64 -Wall -arch arm64 -m64"
    export HSI_XF="-Inolibc -/DSYSV -/DMACOSX -/DMACH64 -/Wall -/m64 -/arch -//arm64"
    export HSI_FF="-O -arch arm64 -m64 -DBLD_KERNEL"
    export HSI_LF="-arch arm64 -m64"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-z"
    ;;

  "macintel")
    export HSI_CF="-O -DSYSV -DMACOSX -DMACINTEL -DMACH64 -Wall -m64 -g"
    export HSI_XF="-Inolibc -/DSYSV -/DMACOSX -/DMACINTEL -/DMACH64 -/Wall -/m64"
    export HSI_FF="-O -m64 -DMACH64 -DBLD_KERNEL"
    export HSI_LF="-m64 -DMACH64"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-z"
    ;;

  "linux64")
    export HSI_CF="-DLINUX -DREDHAT -DPOSIX -DSYSV -DLINUX64 -DMACH64 -Wall -m64"
    export HSI_XF="-Inolibc -w -/m64 -/Wunused"
    export HSI_FF="-m64 -DBLD_KERNEL"
    export HSI_LF="-m64 "
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-Nxz -/Wl,-Bstatic"
    ;;

  "linux" | "redhat")
    export HSI_CF="-O -DLINUX -DREDHAT -DPOSIX -DSYSV -Wall -m32 -Wunused"
    export HSI_XF="-Inolibc -/Wall -/m32"
    export HSI_FF="-O -DBLD_KERNEL -m32"
    export HSI_LF="-m32"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-Nxz -/Wl,-Bstatic"
    ;;

*)
    echo 'Warning in hlib$irafuser.csh: unknown platform '"$MACH"
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
alias mkiraf=${hlib}mkiraf.csh
alias mkmlist=${hlib}mkmlist.csh
alias mkz="${hbin}mkpkg.e $mkzflags"

alias edsym=${hbin}edsym.e
alias generic=${hbin}generic.e
alias mkpkg=${hbin}mkpkg.e
alias rmbin=${hbin}rmbin.e
alias rmfiles=${hbin}rmfiles.e
alias rtar=${hbin}rtar.e
alias wtar=${hbin}wtar.e
alias xc=${hbin}xc.e
alias xyacc=${hbin}xyacc.e
