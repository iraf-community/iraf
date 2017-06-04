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

export XC_CFLAGS="-I${iraf}include ${CFLAGS} -g -Wall -O2"
export HSI_CF="${XC_CFLAGS}"
export HSI_XF="-g -Inolibc -/Wall -/O2"
export HSI_FF="-g -DBLD_KERNEL -O2"
export HSI_LF=""
export HSI_F77LIBS=""
export HSI_LFLAGS=""
export HSI_OSLIBS=""

if [ "$MACH" = "macosx" -o "$MACH" = "linux" ] ; then
    HSI_CF=${HSI_CF}" -m32"
    HSI_XF=${HSI_XF}" -/m32"
    HSI_FF=${HSI_FF}" -m32"
    HSI_LF=${HSI_LF}" -m32"
fi

if [ "$MACH" = "macosx" ] ; then
    export MACOSX_DEPLOYMENT_TARGET=10.5
fi

# The following determines whether or not the VOS is used for filename mapping.
if [ -f ${iraf}lib/libsys.a ]; then
	export	HSI_LIBS="${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a ${hbin}libf2c.a -lm"
else
	export	HSI_CF="$HSI_CF -DNOVOS"
	export	HSI_LIBS="${hlib}libboot.a ${hlib}libos.a -lm"
fi

export HSI_LIBS="$HSI_LIBS $HSI_OSLIBS"

alias	mkiraf=${hlib}mkiraf.sh
alias	mkmlist=${hlib}mkmlist.sh

alias	edsym=${hbin}edsym.e
alias	generic=${hbin}generic.e
alias	mkpkg=${hbin}mkpkg.e
alias	rmbin=${hbin}rmbin.e
alias	rmfiles=${hbin}rmfiles.e
alias	rtar=${hbin}rtar.e
alias	wtar=${hbin}wtar.e
alias	xc=${hbin}xc.e
alias	xyacc=${hbin}xyacc.e
