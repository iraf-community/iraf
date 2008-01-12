# IRAF definitions for the UNIX/bash,zsh user.  The additional variables iraf$ and
# home$ should be defined in the user's .bashrc, .zshrc file.

OS_MACH=`uname -s | tr '[A-Z]' '[a-z]' | cut -c1-6`
if [ "`uname -m`" = "x86_64" ]; then
    MACH="x86_64-linux-generic"
elif [ -f /etc/redhat-release ]; then
    if [ "`uname -m`" = "ppc" ]; then
	MACH="linuxppc"
    else
	MACH="redhat"
    fi
elif [ -f /etc/yellowdog-release -o "`uname -m`" = "ppc" ]; then
    MACH="linuxppc"
else
    MACH="`uname -s | tr '[A-Z]' '[a-z]'`"
fi

if [ "$MACH" = "darwin" ]; then
    # Let the IRAFARCH override the machine to support cross compilation.
    if [ ! "$IRAFARCH" = "" ]; then
        if [ "$IRAFARCH" = "macosx" ]; then
	    MACH="macosx"
        elif [ "$IRAFARCH" = "macintel" ]; then
	    MACH="macintel"
        fi
    else
        if [ "`uname -m`" = "i386" ]; then
            MACH="macintel"
            IRAFARCH="macintel"
        else
            MACH="macosx"
            IRAFARCH="macosx"
        fi
    fi
elif [ "$OS_MACH" = "cygwin" ]; then
    MACH="cygwin"
fi

export MACH OS_MACH

iraf="`echo ${iraf}/ | tr -s '/'`"

hostid="unix"
host="${iraf}unix/"
hlib="${iraf}unix/hlib/"
hbin="${iraf}unix/bin.$MACH/"
hinclude="${iraf}unix/include/"
tmp="/tmp/"

export hostid host hlib hbin hinclude tmp

# Default to GCC for compilation.
CC="gcc"
F77="$hlib/f77.sh"
F2C="$hbin/f2c.e"
RANLIB="ranlib"
XC_CFLAGS="-I$hinclude"

export CC F77 F2C RANLIB XC_CFLAGS

case "$MACH" in
freebsd)
    HSI_CF="-I$hinclude -O -DBSD -Wall"
    HSI_XF="-Inolibc -/DBSD -w -/Wunused"
    HSI_FF="-O"
    HSI_LF="-static"
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    HSI_OSLIBS="-lcompat"
    mkzflags="'lflags=-z' -/static"
    ;;

macosx)
    CC=cc
    CC_f2c=cc
    F2C=$hbin/f2c.e

    HSI_CF="-I$hinclude -O -DMACOSX -Wall  -arch ppc"
    HSI_XF="-Inolibc -/DMACOSX -w -/Wunused"
    if [ "`uname -r`" = "5.5" ]; then
	HSI_CF="$HSI_CF -DOLD_MACOSX"
	HSI_XF="$HSI_XF -DOLD_MACOSX"
    fi
    HSI_FF="-O -arch ppc"
    HSI_LF="-arch ppc"
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    HSI_OSLIBS=""
    export CC_f2c
    mkzflags="'lflags=-z'"
    ;;

macintel)
    CC=cc
    #F77=gcc
    CC_f2c=cc
    F2C=$hbin/f2c.e

    HSI_CF="-I$hinclude -O -DMACOSX -DMACINTEL -Wall -arch i386"
    HSI_FF="-O -arch i386"
    HSI_LF="-arch i386"
    HSI_XF="-Inolibc -/DMACOSX -/DMACINTEL -w -/Wunused"
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    if [ ! "$IRAF_UNIBIN" = "" ]; then
        HSI_CF="$HSI_CF -arch ppc -arch i386"
        HSI_FF="$HSI_FF -arch ppc -arch i386"
        HSI_LF="$HSI_LF"
        HSI_LFLAGS="$HSI_LFLAGS -arch ppc -arch i386"
    fi
    HSI_OSLIBS=""
    export CC_f2c
    mkzflags="'lflags=-z'"
    ;;

linux)
    HSI_CF="-I$hinclude -O -DLINUX -DPOSIX -DSYSV -Wall"
    HSI_XF="-Inolibc -w -/Wunused"
    HSI_FF="-O"
    HSI_LF=""
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    HSI_OSLIBS=""
    mkzflags="'lflags=-Nxz -/Wl,-Bstatic'"
    ;;

redhat)
    HSI_CF="-I$hinclude -O -DLINUX -DREDHAT -DPOSIX -DSYSV -Wall"
    HSI_XF="-Inolibc -w -/Wunused"
    HSI_FF="-O"
    #HSI_LF="-Wl,-Bstatic"
    HSI_LF=""
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    HSI_OSLIBS=""
    mkzflags="'lflags=-Nxz -/Wl,-Bstatic'"
    ;;

sunos)
    HSI_CF="-I$hinclude -O -DSOLARIS -DX86 -DPOSIX -DSYSV -Wall"
    HSI_XF="-Inolibc -w -/Wunused"
    HSI_FF="-O"
    #HSI_LF="-t -Wl,-Bstatic"
    #HSI_LFLAGS="-t -Wl,-Bstatic"
    #HSI_OSLIBS="-lsocket -lnsl -lintl -Wl,-Bdynamic -ldl -Wl,-Bstatic -lelf"
    HSI_LF="-t"
    HSI_F77LIBS=""
    HSI_LFLAGS="-t"
    HSI_OSLIBS="-lsocket -lnsl -lintl -ldl -lelf"
    mkzflags="'lflags=-Nxz -/Wl,-Bstatic'"
    ;;

linuxppc)
    HSI_CF="-I$hinclude -O -DLINUX -DREDHAT -DLINUXPPC -DPOSIX -DSYSV -Wall"
    HSI_XF="-Inolibc -w -/Wunused"
    HSI_FF="-O"
    HSI_LF=""
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    HSI_OSLIBS=""
    mkzflags="'lflags=-Nxz -/Wl,-Bstatic'"
    ;;

cygwin)
    HSI_CF="-I$hinclude -O -DCYGWIN -DLINUX -DREDHAT -DPOSIX -DSYSV -Wall"
    HSI_XF="-Inolibc -w -/Wunused -/DCYGWIN"
    HSI_FF="-O"
    #HSI_LF="-Wl,-Bstatic"
    HSI_LF=""
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    HSI_OSLIBS="${iraf}unix/bin.cygwin/libcompat.a"
    mkzflags="'lflags=-Nxz -/Wl,-Bstatic'"
    ;;

x86_64-linux-generic)
    HSI_CF="-I$hinclude -O -DX86_64 -DLINUX -DPOSIX -DSYSV -Wall"
    HSI_XF="-Inolibc -w -/Wunused"
    HSI_FF="-O"
    HSI_LF=""
    HSI_F77LIBS=""
    HSI_LFLAGS=""
    HSI_OSLIBS=""
    mkzflags="'lflags=-Nxz -/Wl,-Bstatic'"
    ;;

*)
    echo 'Warning in hlib$irafuser.sh: unknown platform '"$MACH"
    exit 1
    ;;
esac

export HSI_CF HSI_XF HSI_FF HSI_LF HSI_F77LIBS HSI_LFLAGS HSI_OSLIBS

# The following determines whether or not the VOS is used for filename mapping.
if [ -f ${iraf}lib/libsys.a ]; then
    HSI_LIBS="${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a"
else
    HSI_CF="$HSI_CF -DNOVOS"
    HSI_LIBS="${hlib}libboot.a ${hlib}libos.a"
fi

HSI_LIBS="$HSI_LIBS $HSI_OSLIBS"
export HSI_LIBS

mkiraf() {
    ${hlib}mkiraf.sh $@
}
mkmlist() {
    ${hlib}mkmlist.sh $@
}
mkz() {
    ${hbin}mkpkg.e "$mkzflags" $@
}
edsym() {
    ${hbin}edsym.e $@
}
generic() {
    ${hbin}generic.e $@
}
mkpkg() {
    ${hbin}mkpkg.e $@
}
rmbin() {
    ${hbin}rmbin.e $@
}
rmfiles() {
    ${hbin}rmfiles.e $@
}
rtar() {
    ${hbin}rtar.e $@
}
wtar() {
    ${hbin}wtar.e $@
}
xc() {
    ${hbin}xc.e $@
}
xyacc() {
    ${hbin}xyacc.e $@
}

