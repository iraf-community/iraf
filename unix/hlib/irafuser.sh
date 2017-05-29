#!/bin/bash
#
# IRAF definitions for the UNIX/bash user.  The additional variables iraf$ and
# home$ should be defined in the user's .login file.


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
  "freebsd")
    export HSI_CF="-O -DBSD -DPOSIX -Wall -Wunused -m32"
    export HSI_XF="-Inolibc -/DBSD -w -/Wunused -/m32"
    export HSI_FF="-O -DBLD_KERNEL -m32"
    export HSI_LF="-static -m32 -B/usr/lib32 -L/usr/lib32"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS="-lcompat"
    #mkzflags="'lflags=-z' -/static"
    ;;

  "macosx")
    export HSI_CF="-O -DMACOSX -Wall -Wunused -arch i386 -m32 -mmacosx-version-min=10.5"
    export HSI_XF="-Inolibc -/DMACOSX -w -/Wunused -/m32 -/arch -//i386"
    export HSI_FF="-O -arch i386 -m32 -DBLD_KERNEL -mmacosx-version-min=10.5"
    export HSI_LF="-arch i386 -m32 -mmacosx-version-min=10.5"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-z"
    ;;

  "macintel")
    export HSI_CF="-O -DMACOSX -DMACINTEL -DMACH64 -Wall -Wunused -m64 -g"
    export HSI_XF="-Inolibc -/DMACOSX -/DMACINTEL -w -/Wunused -/DMACH64 -/m64"
    export HSI_FF="-O -m64 -DMACH64 -DBLD_KERNEL"
    export HSI_LF="-m64 -DMACH64"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-z"
    ;;

  "ipad")
    export XC_CFLAGS="-I/var/include"
    export HSI_CF="-O -I/var/include -DMACOSX -DMACINTEL -DIPAD -Wall -Wunused"
    export HSI_XF="-Inolibc -/DMACOSX -/DMACINTEL -/DIPAD -w -/Wunused"
    export HSI_FF="-O -DBLD_KERNEL"
    export HSI_LF=""
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-z"
    ;;

  "linux64")
    export HSI_CF="-g -DLINUX -DREDHAT -DPOSIX -DSYSV -DLINUX64 -DMACH64 -Wall -m64"
    export HSI_XF="-g -Inolibc -w -/m64 -/Wunused"
    export HSI_FF="-g -m64 -DBLD_KERNEL"
    export HSI_LF="-m64 "
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-Nxz -/Wl,-Bstatic"
    ;;

  "linux" | "redhat")
    export HSI_CF="-O -DLINUX -DREDHAT -DPOSIX -DSYSV -w -m32 -Wunused"
    export HSI_XF="-Inolibc -w -/Wunused -/m32"
    export HSI_FF="-O -DBLD_KERNEL -m32"
    export HSI_LF="-m32"
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS=""
    #mkzflags="lflags=-Nxz -/Wl,-Bstatic"
    ;;

  "sunos")
    export HSI_CF="-O -DSOLARIS -DX86 -DPOSIX -DSYSV -w -Wunused"
    export HSI_XF="-Inolibc -w -/Wunused"
    export HSI_FF="-O"
    #export HSI_LF="-t -Wl,-Bstatic"
    #export HSI_LFLAGS="-t -Wl,-Bstatic"
    #export HSI_OSLIBS=\
    #	"-lsocket -lnsl -lintl -Wl,-Bdynamic -ldl -Wl,-Bstatic -lelf"
    export HSI_LF="-t"
    export HSI_F77LIBS=""
    export HSI_LFLAGS="-t"
    export HSI_OSLIBS="-lsocket -lnsl -lintl -ldl -lelf"
    #mkzflags="lflags=-Nxz -/Wl,-Bstatic"
    ;;

  "cygwin")
    export HSI_CF="-O -DCYGWIN -DLINUX -DREDHAT -DPOSIX -DSYSV -w -Wunused"
    export HSI_XF="-Inolibc -w -/Wunused -/DCYGWIN"
    export HSI_FF="-O"
    #export HSI_LF="-Wl,-Bstatic"
    export HSI_LF=""
    export HSI_F77LIBS=""
    export HSI_LFLAGS=""
    export HSI_OSLIBS="${iraf}unix/bin.cygwin/libcompat.a"
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
export HSI_CF="-I${iraf}include $HSI_CF"
export HSI_FF="-I${iraf}include $HSI_FF"
export HSI_LF="-I${iraf}include $HSI_LF"
export HSI_XF="-I${iraf}include $HSI_XF"
export XC_CFLAGS="-I${iraf}include"


# The following determines whether or not the VOS is used for filename mapping.
if [ -f ${iraf}lib/libsys.a ]; then
	export	HSI_LIBS="${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a ${hbin}libf2c.a -lm"
else
	export	HSI_CF="$HSI_CF -DNOVOS"
	export	HSI_LIBS="${hlib}libboot.a ${hlib}libos.a"
fi

export HSI_LIBS="$HSI_LIBS $HSI_OSLIBS"

alias	mkiraf=${hlib}mkiraf.csh
alias	mkmlist=${hlib}mkmlist.csh
#alias	mkz=${hbin}mkpkg.e "$mkzflags"

alias	edsym=${hbin}edsym.e
alias	generic=${hbin}generic.e
alias	mkpkg=${hbin}mkpkg.e
alias	rmbin=${hbin}rmbin.e
alias	rmfiles=${hbin}rmfiles.e
alias	rtar=${hbin}rtar.e
alias	wtar=${hbin}wtar.e
alias	xc=${hbin}xc.e
alias	xyacc=${hbin}xyacc.e
