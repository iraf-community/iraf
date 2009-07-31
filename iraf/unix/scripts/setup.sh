#!/bin/sh

########################################################################
########################################################################

set_irafenv() {
  #
  IRAFARCH="$MACH"
  export IRAFARCH
  #
  if [ "$iraf" = "" ]; then
    iraf="${PREFIX}/iraf/iraf/"
  fi
  export iraf
  #
  host="${iraf}${HOSTID}/"
  export host
  #
  hconfig="${host}config/"
  hlib="${host}lib/"
  hbin="${host}bin/"
  hscripts="${host}scripts/"
  hinclude="${host}include/"
  tmp="/tmp/"
  export hconfig hlib hbin hscripts hinclude tmp
  #
  CC="gcc"
  F77="${hscripts}f77.sh"
  F2C="${hbin}f2c.e"
  RANLIB="ranlib"
  export CC F77 F2C RANLIB
  #
  # basic settings
  #
  HSI_CF="-Wall -I$hinclude -DPREFIX=\\\"$PREFIX\\\""
  HSI_XF="-Wall"
  HSI_FF="-Wall"
  HSI_LF=""
  HSI_F77LIBS=""
  HSI_LFLAGS=""
  HSI_OSLIBS=""
  #
  XC_CFLAGS="-Wall"
  XC_FFLAGS="-Wall"
  #XC_FFLAGS="-Ns1602 -Nx512"
  XC_LIBS="-lf2c"
  #
  XC_XLFLAGS=""
  #
  # Architecture-dependent settings
  #
  F=""
  case "$ARCHITECTURE" in
  i386)
    SPP_BYTE_ENDIAN="little"
    SPP_FLOAT_ENDIAN="little"
    F="$F -DI386"
    ;;
  x86_64)
    SPP_BYTE_ENDIAN="little"
    SPP_FLOAT_ENDIAN="little"
    F="$F -DX86_64"
    #XC_XLFLAGS="$XC_XLFLAGS -/mcmodel=medium"
    ;;
  powerpc)
    SPP_BYTE_ENDIAN="little"
    SPP_FLOAT_ENDIAN="little"
    F="$F -DPOWERPC"
    ;;
  sparc)
    SPP_BYTE_ENDIAN="big"
    SPP_FLOAT_ENDIAN="big"
    F="$F -DSPARC"
    ;;
  *)
    echo "[ERROR] Unknown Architecture: $ARCHITECTURE"
    echo "To support new architecture, edit iraf/unix/scripts/setup.sh and"
    echo "modify source files in iraf/unix/{os,sys,include/iraf} directories."
    exit 1
    ;;
  esac
  #
  HSI_SPP_MODEL_CDEF=""
  if [ "$SPP_DATA_MODEL" = "lp64" ]; then
    HSI_SPP_MODEL_CDEF="-DSPP_LP64"
  elif [ "$SPP_DATA_MODEL" = "ilp64" ]; then
    HSI_SPP_MODEL_CDEF="-DSPP_ILP64"
  fi
  F="$F $HSI_SPP_MODEL_CDEF"
  HSI_CF="$HSI_CF $F"
  XC_CFLAGS="$XC_CFLAGS $F"
  XC_FFLAGS="$XC_FFLAGS $F"
  #
  # OS-dependent settings
  #
  case "$OPERATING_SYSTEM" in
  linux)
    CF_DEFS="-DLINUX -DPOSIX -DSYSV"
    HSI_CF="$HSI_CF -O $CF_DEFS"
    XC_CFLAGS="$XC_CFLAGS -O $CF_DEFS"
    XC_FFLAGS="$XC_FFLAGS -O"
    XC_XLFLAGS="$XC_XLFLAGS -Nz"
    XC_LIBS="$XC_LIBS -lm"
    ;;
  freebsd)
    CF_DEFS="-DBSD -DPOSIX"
    HSI_CF="$HSI_CF -O $CF_DEFS"
    XC_CFLAGS="$XC_CFLAGS -O $CF_DEFS"
    XC_FFLAGS="$XC_FFLAGS -O"
    XC_XLFLAGS="$XC_XLFLAGS -z -/static"
    XC_LIBS="$XC_LIBS -lm -lcompat"
    ;;
  darwin)
    CF_DEFS="-DMACOSX -DPOSIX"
    # note: disabled "-O" option for MacOSX.
    HSI_CF="$HSI_CF $CF_DEFS"
    XC_CFLAGS="$XC_CFLAGS $CF_DEFS"
    XC_FFLAGS="$XC_FFLAGS"
    XC_XLFLAGS="$XC_XLFLAGS -Nz"
    XC_LIBS="$XC_LIBS -lm"
    ;;
  sunos)
    CF_DEFS="-DSOLARIS -DPOSIX -DSYSV"
    HSI_CF="$HSI_CF -O $CF_DEFS"
    XC_CFLAGS="$XC_CFLAGS -O $CF_DEFS"
    XC_FFLAGS="$XC_FFLAGS -O"
    XC_XLFLAGS="$XC_XLFLAGS -Nz"
    XC_LIBS="$XC_LIBS -lm -lsocket -lnsl -lintl -ldl -lelf"
    ;;
  cygwin)
    CF_DEFS="-DCYGWIN -DPOSIX -DSYSV"
    HSI_CF="$HSI_CF -O $CF_DEFS"
    XC_CFLAGS="$XC_CFLAGS -O $CF_DEFS"
    XC_FFLAGS="$XC_FFLAGS -O"
    XC_XLFLAGS="$XC_XLFLAGS -Nz"
    XC_LIBS="$XC_LIBS -lm"
    ;;
  *)
    echo "[ERROR] Unknown operating system: $OPERATING_SYSTEM"
    echo "To support new operating system, edit iraf/unix/scripts/setup.sh and"
    echo "modify source files in iraf/unix/{os,sys,include/iraf} directories."
    exit 1
    ;;
  esac
  #
  if [ "$1" = "novos" ]; then
    HSI_CF="$HSI_CF -DNOVOS"
    HSI_LIBS="${hlib}libboot.a ${hlib}libos.a"
  else
    HSI_LIBS="${hlib}libboot.a ${iraf}lib/libsys.a ${iraf}lib/libvops.a ${hlib}libos.a"
  fi
  export HSI_CF HSI_XF HSI_FF HSI_LF HSI_F77LIBS HSI_LFLAGS HSI_OSLIBS
  export HSI_LIBS
  export XC_CFLAGS XC_FFLAGS XC_XLFLAGS XC_LIBS

  # see tables/lib/zzsetenv.def
  #tables=${iraf}tables/
  #tablesbin=${tables}bin/
  #tableslib=${tables}lib/
  #export tables tablesbin tableslib

  # see noao/lib/zzsetenv.def
  #noao=${iraf}noao/
  #noaobin=${noao}bin/
  #noaolib=${noao}lib/
  #export noao noaobin noaolib

  cincludes="$hinclude ${iraf}tables/base/tbtables/cfitsio/"
  export cincludes
  #pkglibs=${noaobin},${noaolib},${tablesbin},${tableslib}
  #export pkglibs

  #PATH=${PREFIX}/iraf/local/bin:$PATH
}

########################################################################

set_mach () {

  ARG_MACH="`echo $1 | tr '[A-Z]' '[a-z]'`"
  
  if [ "$ARG_MACH" = "auto" ]; then
    #
    UNAME_P="`uname -p | tr '[A-Z]' '[a-z]'`"
    if [ "$UNAME_P" = "unknown" -o "$UNAME_P" = "" ]; then
      UNAME_P="`uname -m | tr '[A-Z]' '[a-z]'`"
    fi
    ARCHITECTURE="`echo $UNAME_P | sed -e 's/^i[3456]86$/i386/' -e 's/\(^cygwin\)\(_.*\)/\1/'`"
    OPERATING_SYSTEM="`uname -s | tr '[A-Z]' '[a-z]'`"
    VENDOR="generic"
    #
  else
    #
    ARCHITECTURE="`echo $ARG_MACH | sed -e 's/-.*//'`"
    OPERATING_SYSTEM="`echo $ARG_MACH | sed -e 's/^[^-]*-//' -e 's/-.*//'`"
    VENDOR="`echo $ARG_MACH | sed -e 's/^.*-//'`"
    #
  fi

  #         ILP32  LP64  LLP64 ILP64
  # short    16     16    16    16
  # int      32     32    32    64
  # long     32     64    32    64
  # pointer  32     64    64    64
  #
  # IRAF's default data model of SPP is ILP32. Set LP64 for 64-bit OS.
  # ILP64 is used for development only.

  if [ "$ARCHITECTURE" = "x86_64" ]; then
    SPP_DATA_MODEL="lp64"
    #SPP_DATA_MODEL="ilp64"
  else
    SPP_DATA_MODEL="ilp32"
  fi
  
  MACH="${ARCHITECTURE}-${OPERATING_SYSTEM}-${VENDOR}"
}

########################################################################

set_config () {

  # zsvjmp.s settings
  #if [ -f ${host}as.${MACH}/zsvjmp.${SPP_DATA_MODEL}.s ]; then
  #  ( cd ${host}as.${MACH} ; rm -f zsvjmp.s ; ln -s zsvjmp.${SPP_DATA_MODEL}.s zsvjmp.s )
  #fi
  # mach.h, iraf.h settings
  #if [ -f ${hconfig}iraf.${SPP_DATA_MODEL}.h ]; then
    #( cd ${hconfig} ; rm -f iraf.h ; ln -s iraf.${SPP_DATA_MODEL}.h iraf.h )
    #( cd ${hconfig} ; rm -f mach.h ; ln -s mach.${SPP_DATA_MODEL}.h mach.h )
  #  ( cd ${hconfig} ; rm -f f2c.h ; ln -s f2c.${SPP_DATA_MODEL}.h f2c.h )
    #( cd ${hconfig} ; rm -f entxkw.f ; ln -s entxkw.${SPP_DATA_MODEL}.f entxkw.f )
  #else
  #  echo "[ERROR] No such data model: ${SPP_DATA_MODEL}"
  #  exit 1
  #fi

  return
}

########################################################################

output_makefile () {

  ARG_DIR=$1
  ARG_INFILE=$2
  ARG_ADDITIONAL_DEFS=$3

  echo Making $ARG_DIR/Makefile.
  cat <<EOF > $ARG_DIR/Makefile

HOSTID      = $HOSTID
SITEID      = $SITEID
CC          = $CC
F77         = $F77
F2C         = $F2C
HSI_CF      = $HSI_CF
HSI_FF      = $HSI_FF
HSI_LF      = $HSI_LF
HSI_F77LIBS = $HSI_F77LIBS
RANLIB      = $RANLIB
HSI_LIBS    = $HSI_LIBS
$ARG_ADDITIONAL_DEFS

EOF

  cat $ARG_DIR/$ARG_INFILE >> $ARG_DIR/Makefile

}

########################################################################

install_file() {

  F="$1"
  if [ ! -d $F ]; then
    echo Installing $F
    D=`echo ./$F | sed -e 's/\/[^\/]*$//'`
    if [ ! -d $DESTDIR/$PREFIX/iraf/$D ]; then
      mkdir -p -m 755 $DESTDIR/$PREFIX/iraf/$D
    fi
    cp -p $F $DESTDIR/$PREFIX/iraf/$D/.
  fi

}

########################################################################
#  M A I N
########################################################################

LANG=C

HOSTID="unix"
SITEID="noao"

if [ "$1" != "" ]; then
  COMMAND=$1
else
  COMMAND="set_env"
fi
if [ "$2" != "" ]; then
  ARG_MACH=$2
fi
if [ "$3" != "" ]; then
  PREFIX=$3
fi
#
if [ "$4" != "" ]; then
  BINDIR=$4
fi
if [ "$5" != "" ]; then
  DESTDIR=$5
fi

ARCHITECTURE=""
OPERATING_SYSTEM=""
VENDOR=""
SPP_DATA_MODEL=""
SPP_BYTE_ENDIAN=""
SPP_FLOAT_ENDIAN=""

USER=`whoami`

################################

if [ "$ARG_MACH" = "" ]; then
  set_mach "$MACH"
else
  set_mach "$ARG_MACH"
fi

export ARCHITECTURE OPERATING_SYSTEM VENDOR
export SPP_DATA_MODEL SPP_BYTE_ENDIAN SPP_FLOAT_ENDIAN

#echo debug: $ARCHITECTURE :: $OPERATING_SYSTEM :: $VENDOR
#echo debug: $MACH
#echo $ARCHITECTURE

if [ "`echo $COMMAND | grep '^make_check_'`" != "" ]; then
  COMMAND="`echo $COMMAND | sed -e 's/^make_check_/make_/'`"
  F2C_AUTO_INCLUDE="true"
  export F2C_AUTO_INCLUDE
fi

case "$COMMAND" in
"set_env")
  #
  set_irafenv vos
  #
  ;;

"boot_makefiles")
  #
  iraf="`pwd`/iraf/"
  set_irafenv novos
  set_config
  #
  echo "Architecture   : $ARCHITECTURE"
  echo "OS             : $OPERATING_SYSTEM"
  echo "Vendor         : $VENDOR"
  echo "SPP Data Model : $SPP_DATA_MODEL"
  sleep 1
  #
  #mkdir -p iraf/unix/bin
  #mkdir -p iraf/unix/include
  #mkdir -p iraf/unix/lib
  chmod 755 iraf/unix/scripts/*.sh
  #
  #( cd iraf/unix/include ; rm -f iraf ; ln -s ../hlib/libc iraf )
  #( cd iraf/unix/include ; rm -f iraf.h ; ln -s ../hlib/libc/iraf.h . )
  #( cd iraf/unix/include ; rm -f f2c.h ; ln -s ../config/f2c.h . )
  #( cd iraf/unix/f2c/src ; rm -f f2c.h ; ln -s ../../include/f2c.h . )
  #( cd iraf/unix/f2c/libf2c ; rm -f f2c.h ; ln -s ../../include/f2c.h . )
  #( cd iraf/unix/boot/spp/rpp/rppfor ; rm -f entxkw.f ; ln -s ../../../../config/entxkw.f . )
  #
  output_makefile iraf/unix/include makefile.in
  #
  F="iraf/unix/config/Makefile"
  D="DEFS ="
  cat <<EOF | cpp -P $HSI_CF > $F 
#include <iraf/endian.h>
#if BYTE_ORDER == LITTLE_ENDIAN
X_MARK_X foo;
#endif
EOF
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  if [ "`grep 'X_MARK_X' $F`" != "" ]; then
    if [ "$SPP_BYTE_ENDIAN" != "little" ]; then
      echo "[ERROR] Cannot detect endian correctly."
      exit 1
    fi
    D="$D -DBYTE_LITTLE"
  fi
  cat <<EOF | cpp -P $HSI_CF > $F
#include <iraf/endian.h>
#if FLOAT_WORD_ORDER == LITTLE_ENDIAN
X_MARK_X foo;
#endif
EOF
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  if [ "`grep 'X_MARK_X' $F`" != "" ]; then
    if [ "$SPP_FLOAT_ENDIAN" != "little" ]; then
      echo "[ERROR] Cannot detect endian correctly."
      exit 1
    fi
    D="$D -DFLOAT_LITTLE"
  fi
  output_makefile iraf/unix/config makefile.in "$D"
  #
  # F2C
  #F="`echo $HSI_CF | tr ' ' '\n' | egrep -e '-DSPP' -e '-I' | tr '\n' ' '`"
  # See also MAX_OUTPUT_SIZE in niceprintf.h
  F="-DDEF_C_LINE_LENGTH=5120 $HSI_SPP_MODEL_CDEF"
  #
  echo Makeing iraf/unix/f2c/src/Makefile.
  ( cd iraf/unix/f2c/src    ; cat makefile.u | \
    sed -e 's|^\(CFLAGS = \)\(.*\)|\1\2 -Wall '"$F"'|' \
        -e 's|^\(CC = \)\(.*\)|\1gcc|' > Makefile )
  #
  echo Makeing iraf/unix/f2c/libf2c/Makefile.
  if [ "$SPP_DATA_MODEL" = "lp64" ]; then
    ( cd iraf/unix/f2c/libf2c ; cat makefile.u | \
    sed -e 's/^\(OFILES = \)\(.*\)/\1$(QINT) \2/' \
        -e 's|^\(CFLAGS = \)\(.*\)|\1\2 -Wall '"$F"'|' \
        -e 's|^\(CC = \)\(.*\)|\1gcc|' > Makefile )
  else
    ( cd iraf/unix/f2c/libf2c ; cat makefile.u | \
    sed -e 's|^\(CFLAGS = \)\(.*\)|\1\2 -Wall '"$F"'|' \
        -e 's|^\(CC = \)\(.*\)|\1gcc|' > Makefile )
  fi
  #
  output_makefile iraf/unix/os makefile.in
  output_makefile iraf/unix/gdev/sgidev makefile.in
  output_makefile iraf/unix/boot/bootlib makefile.in
  output_makefile iraf/unix/boot/generic makefile.in
  output_makefile iraf/unix/boot/mkpkg makefile.in
  output_makefile iraf/unix/boot/rmbin makefile.in
  output_makefile iraf/unix/boot/rmfiles makefile.in
  output_makefile iraf/unix/boot/rtar makefile.in
  output_makefile iraf/unix/boot/wtar makefile.in
  output_makefile iraf/unix/boot/spp makefile.in
  output_makefile iraf/unix/boot/spp/xpp makefile.in
  output_makefile iraf/unix/boot/spp/rpp/rppfor makefile.in
  output_makefile iraf/unix/boot/spp/rpp/ratlibf makefile.in
  output_makefile iraf/unix/boot/spp/rpp/ratlibc makefile.in
  output_makefile iraf/unix/boot/spp/rpp makefile.in
  output_makefile iraf/unix/boot/xyacc makefile.in
  output_makefile iraf/unix/gdev/sgidev makefile.in
  #
  ;;

"make_iraf")
  #
  iraf="`pwd`/iraf/"
  set_irafenv novos
  set_config
  #
  mkdir -p tmp_bin
  ( cd tmp_bin
  for i in `ls ../iraf/unix/bin/ | grep '\.e$'` ; do
    F=`echo $i | sed -e 's/\.e$//'`
    rm -f $F
    ln -s ../iraf/unix/bin/$i $F
  done
  )
  #
  PATH="`pwd`/tmp_bin:$PATH"
  export PATH
  ( cd iraf ; mkpkg )
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  #
  # making helpdb
  if [ "$F2C_AUTO_INCLUDE" != "true" -a -x iraf/bin/cl.e ]; then
    iraf="`pwd`/iraf/" host="${iraf}unix/" hconfig="${host}config/" TERM=vt100 `pwd`/iraf/bin/cl.e <<EOF

softools
mkhelpdb base\$root.hd base\$helpdb.mip
mkhelpdb tables\$base/root.hd tables\$base/helpdb.mip
mkhelpdb noao\$base/root.hd noao\$base/helpdb.mip
logout
EOF
    rm -f clhistory.txt uparmsosmkhelb.par
  fi
  #
  ;;

"reboot_makefiles")
  #
  iraf="`pwd`/iraf/"
  set_irafenv vos
  set_config
  #
  output_makefile iraf/unix/os makefile.in
  output_makefile iraf/unix/gdev/sgidev makefile.in
  output_makefile iraf/unix/boot/bootlib makefile.in
  output_makefile iraf/unix/boot/generic makefile.in
  output_makefile iraf/unix/boot/mkpkg makefile.in
  output_makefile iraf/unix/boot/rmbin makefile.in
  output_makefile iraf/unix/boot/rmfiles makefile.in
  output_makefile iraf/unix/boot/rtar makefile.in
  output_makefile iraf/unix/boot/wtar makefile.in
  output_makefile iraf/unix/boot/spp makefile.in
  output_makefile iraf/unix/boot/spp/xpp makefile.in
  output_makefile iraf/unix/boot/spp/rpp/rppfor makefile.in
  output_makefile iraf/unix/boot/spp/rpp/ratlibf makefile.in
  output_makefile iraf/unix/boot/spp/rpp/ratlibc makefile.in
  output_makefile iraf/unix/boot/spp/rpp makefile.in
  output_makefile iraf/unix/boot/xyacc makefile.in
  output_makefile iraf/unix/gdev/sgidev makefile.in
  #
  ;;

"make_tables")
  #
  iraf="`pwd`/iraf/"
  set_irafenv vos
  set_config
  #
  mkdir -p tmp_bin
  ( cd tmp_bin
  for i in `ls ../iraf/unix/bin/ | grep '\.e$'` ; do
    F=`echo $i | sed -e 's/\.e$//'`
    rm -f $F
    ln -s ../iraf/unix/bin/$i $F
  done
  )
  #
  PATH="`pwd`/tmp_bin:$PATH"
  export PATH
  extlibs="${iraf}tables/lib/"
  export extlibs
  ( cd iraf/tables ; tables="${iraf}tables/" mkpkg -p tables SPPFITSIO=yes )
  #( cd iraf/tables/base ; for i in `ls|grep '\.a$'`; do mv $i ../lib/.; done )
  ;;

"make_noao")
  #
  iraf="`pwd`/iraf/"
  set_irafenv vos
  set_config
  #
  mkdir -p tmp_bin
  ( cd tmp_bin
  for i in `ls ../iraf/unix/bin/ | grep '\.e$'` ; do
    F=`echo $i | sed -e 's/\.e$//'`
    rm -f $F
    ln -s ../iraf/unix/bin/$i $F
  done
  )
  #
  PATH="`pwd`/tmp_bin:$PATH"
  export PATH
  extlibs="${iraf}/noao/lib/,${iraf}tables/lib/"
  export extlibs
  tables="${iraf}tables/"
  export tables
  ( cd iraf/noao ; noao="${iraf}noao/" mkpkg -p noao )
  #( cd iraf/noao/base ; for i in `ls|grep '\.a$'`; do mv $i ../lib/. ; done )
  ;;

"make_install")
  #
  mkdir -p $DESTDIR/$PREFIX/iraf
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  #
  for i in COPYRIGHTS iraf/tags \
           iraf/unix/README \
           iraf/unix/bin/alloc.e iraf/unix/bin/sgi*  \
           iraf/unix/scripts/setup.sh \
           iraf/bin \
           iraf/unix/config \
           iraf/base iraf/config iraf/dev \
           iraf/doc iraf/math iraf/sys \
           iraf/pkg iraf/tables iraf/noao ; do
    L=`find $i -print | egrep -v -e '\.[acfhlorxy]$' -e '\.[fhx]\....$' -e '\.inc$' -e '\.inc\.orig$' -e '\.com$' -e '\.bd$' -e '\.gy$' -e '\.gh$' -e '\.gc$' -e '\.gx$' -e '\.gx\.old$' -e '/omkpkg$' -e '/mkpkg$' -e '/mkpkg\.[^e][^/]*$' -e '/[mM]akefile[^/]*$' -e '/configure[^/]*$' -e '/strip\.[^/]*$' -e '/strip$' | egrep -v -e '~$' -e '/\.svn'`
    for j in $L ; do
      install_file $j
    done
  done
  #
  install_file iraf/unix/include/f2c.h
  install_file iraf/unix/boot/spp/rpp/rppfor/entxkw.f
  install_file iraf/unix/config/mkpkg.inc
  install_file iraf/unix/config/iraf.h
  install_file iraf/unix/config/mach.h
  #
  mkdir -p -m 755 $DESTDIR/$PREFIX/iraf/iraf/config
  mkdir -p -m 777 $DESTDIR/$PREFIX/iraf/imdirs
  #
  mkdir -p $DESTDIR/dev
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  if [ "$USER" = "root" ]; then
    ( cd $DESTDIR/dev
      echo "Creating /dev/imtli"
      rm -f imtli ; mknod -m 777 imtli p
      echo "Creating /dev/imtlo"
      rm -f imt1o ; mknod -m 777 imt1o p
      rm -f imt1 ; ln -s imt1o imt1
    )
    S=$?
    if [ ! $S = 0 ]; then
      exit $S
    fi
  fi
  #
  mkdir -p $DESTDIR/$BINDIR
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  #
  cat iraf/unix/scripts/mkiraf.sh | \
      sed -e 's|^HOSTID=.*|HOSTID='"$HOSTID"'|' -e 's|^PREFIX=.*|PREFIX='"$PREFIX"'|' > $DESTDIR/$BINDIR/mkiraf
  cat iraf/unix/scripts/cl.sh | \
      sed -e 's|^HOSTID=.*|HOSTID='"$HOSTID"'|' -e 's|^PREFIX=.*|PREFIX='"$PREFIX"'|' -e 's|^MACH=.*|MACH='"$MACH"'|' > $DESTDIR/$BINDIR/cl
  chmod 755 $DESTDIR/$BINDIR/mkiraf
  chmod 755 $DESTDIR/$BINDIR/cl
  if [ "$USER" = "root" ]; then
    chown root:root $DESTDIR/$BINDIR/mkiraf
    chown root:root $DESTDIR/$BINDIR/cl
  fi
  for i in sgidispatch ; do
    F=$DESTDIR/$BINDIR/$i
    echo '#!/bin/sh' > $F
    echo '' >> $F
    echo "PREFIX=$PREFIX" >> $F
    echo "MACH=$MACH" >> $F
    echo ". $PREFIX/iraf/iraf/unix/scripts/setup.sh" >> $F
    echo "exec $PREFIX/iraf/iraf/unix/bin/$i.e \$@" >> $F
    chmod 755 $F
    if [ "$USER" = "root" ]; then
      chown root:root $F
    fi
  done
  #
  if [ "$USER" = "root" ]; then
    chown -R root:root $DESTDIR/$PREFIX/iraf/iraf
    chown root:root $DESTDIR/$PREFIX/iraf/COPYRIGHTS
    chown root:root $DESTDIR/$PREFIX/iraf/imdirs
    chown root:root $DESTDIR/$PREFIX/iraf
    S=$?
    if [ ! $S = 0 ]; then
      exit $S
    fi
  fi
  ;;

"make_install_devel")
  #
  mkdir -p $DESTDIR/$PREFIX/iraf
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  #
  for i in iraf/mkpkg \
           iraf/unix/config \
           iraf/base iraf/config iraf/dev \
           iraf/doc iraf/math iraf/sys \
           iraf/pkg iraf/tables iraf/noao ; do
    L=`find $i -print | egrep -e '\.[acfhlorxy]$' -e '\.[fhx]\....$' -e '\.inc$' -e '\.inc\.orig$' -e '\.com$' -e '\.bd$' -e '\.gy$' -e '\.gh$' -e '\.gc$' -e '\.gx$' -e '\.gx\.old$' -e '/omkpkg$' -e '/mkpkg$' -e '/mkpkg\.[^e][^/]*$' -e '/[mM]akefile[^/]*$' -e '/configure[^/]*$' -e '/strip\.[^/]*$' -e '/strip$' | egrep -v -e '\.[aoe]$' | egrep -v -e 'unix/config/mkpkg.inc$' -e 'unix/config/iraf.h$' -e 'unix/config/mach.h$' -e '~$' -e '/\.svn'`
    for j in $L ; do
      install_file $j
    done
  done
  for i in INSTALL Makefile \
           iraf/unix/as* iraf/unix/boot iraf/unix/f2c \
           iraf/unix/gdev iraf/unix/include iraf/unix/mkpkg \
           iraf/unix/mkpkg.sh iraf/unix/os iraf/unix/portkit \
           iraf/unix/scripts iraf/unix/shlib iraf/unix/sun ; do
    L=`find $i -print | egrep -v -e 'unix/scripts/setup.sh$' -e 'unix/include/f2c.h$' -e 'unix/boot/spp/rpp/rppfor/entxkw.f$' -e '\.[aoe]$' | egrep -v -e '~$' -e '/\.svn'`
    for j in $L ; do
      install_file $j
    done
  done
  for i in iraf/unix/bin \
           iraf/unix/lib iraf/lib iraf/noao/lib iraf/tables/lib ; do
    L=`find $i -print | egrep -v -e 'unix/bin/alloc\.e$' -e 'unix/bin/sgi.*$' | egrep -v -e '~$' -e '/\.svn'`
    for j in $L ; do
      install_file $j
    done
  done
  #
  mkdir -p -m 755 $DESTDIR/$PREFIX/iraf/iraf/config
  #
  mkdir -p $DESTDIR/$BINDIR
  S=$?
  if [ ! $S = 0 ]; then
    exit $S
  fi
  #
  ( cd $DESTDIR/$BINDIR/ ; rm -f mkmlist ; \
    ln -sf $PREFIX/iraf/iraf/unix/scripts/mkmlist.sh . )
  for i in generic mkpkg rmbin rmfiles rpp rtar wtar xc xpp xyacc; do
    F=$DESTDIR/$BINDIR/$i
    echo '#!/bin/sh' > $F
    echo '' >> $F
    echo "PREFIX=$PREFIX" >> $F
    echo "MACH=$MACH" >> $F
    echo ". $PREFIX/iraf/iraf/unix/scripts/setup.sh" >> $F
    echo "exec $PREFIX/iraf/iraf/unix/bin/$i.e \$@" >> $F
    chmod 755 $F
    if [ "$USER" = "root" ]; then
      chown root:root $F
    fi
  done
  #
  if [ "$USER" = "root" ]; then
    chown -R root:root $DESTDIR/$PREFIX/iraf/iraf
    chown root:root $DESTDIR/$PREFIX/iraf/COPYRIGHTS
    chown root:root $DESTDIR/$PREFIX/iraf/imdirs
    chown root:root $DESTDIR/$PREFIX/iraf
    S=$?
    if [ ! $S = 0 ]; then
      exit $S
    fi
  fi
  ;;

*)
  echo "[setup.sh] Unknown command: $COMMAND"
  exit 1
  ;;
esac

