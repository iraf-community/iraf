# configure.ac for CFITSIO

# Package name and version number:
m4_define([v_maj], [4])
m4_define([v_min], [6])
m4_define([v_mic], [2])
m4_define([project_version], [v_maj.v_min.v_mic])
AC_INIT([cfitsio], [project_version])
#AC_INIT([cfitsio], [project_version], [ccfits@heasarc.gsfc.nasa.gov], [tarname], [url])

# Shared library versioning, passed to the linker using
# "-version-info lt_cur:lt_rev:lt_age", where:
# lt_cur = current: The most recent interface number that this
#          library implements.
# lt_rev = revision: The implementation number of the current interface.
# lt_age = age: The difference between the newest and oldest interfaces
#          that this library implements. In other words, the library
#          implements all the interface numbers in the range from number
#          current - age to current.

m4_define([lt_cur], [10])
#m4_define([lt_rev], [v_mic])
#m4_define([lt_age], [v_min])
#version_info="lt_cur:lt_rev:lt_age"
version_info="lt_cur"

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIRS([m4])

AC_CANONICAL_TARGET([])

#AC_CONFIG_HEADERS(config.h)
AC_CONFIG_SRCDIR([fitscore.c])
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE

LT_INIT

AC_SUBST([version_info])

AC_PROG_LN_S

#--------------------------------------------------------------------
# System type
#--------------------------------------------------------------------

case $host in
    *cygwin*)
	ARCH="cygwin"
	EXT="cygwin"
	;;
    *apple-darwin*)
	# Darwin can be powerpc, i386, or x86_64
	ARCH=`uname -p`
	EXT="darwin"
	;;
    *freebsd*)
	ARCH="linux"
	EXT="lnx"
	;;
    *haiku*)
        # Haiku can be arm, i386, or x86_64
        ARCH=`uname -p`
        EXT="haiku"
        ;;
    *hpux*)
	ARCH="hp"
	EXT="hpu"
	;;
    *irix*)
	ARCH="sgi"
	EXT="sgi"
	;;
    *linux*)
	ARCH="linux"
	EXT="lnx"
	;;
    *mingw32*)
	#ARCH=""
	EXT="mingw32"
	;;
    *osf1*)
	ARCH="alpha"
	EXT="osf"
	;;
    *solaris*)
	ARCH="solaris"
	EXT="sol"
	;;
    *ultrix*)
	ARCH="dec"
	EXT="dec"
	;;
    *)
	echo "cfitsio: == Don't know what to do with $host"
	;;
esac

#--------------------------------------------------------------------
# Try first to find a proprietary C compiler, then gcc
#--------------------------------------------------------------------
# This section needs to precede the command options since e.g. the
# bzip2 CHECK_LIB test needs a compiler and in the process sets "CC"
# if it isn't already set.

if test "x$EXT" = xdarwin; then
  # Prefer clang on Darwin:
  AC_PROG_CC([clang cc gcc])
else
  AC_PROG_CC
  #if test "x$EXT" != xcygwin && test "x$EXT" != xlnx && test "x$EXT" != xmingw32; then
  #    AC_CHECK_PROGS(CC, cc)
  #fi
fi

#--------------------------------------------------------------------
# Command options
#--------------------------------------------------------------------

ADD_CURL=yes
AC_ARG_ENABLE(
  curl,
  [AS_HELP_STRING([--disable-curl],[Disable linking with the curl library. Disables remote file i/o support])],
  [ if test $enableval = no; then ADD_CURL=no; fi ]
)
if test "x$ADD_CURL" = xno; then
  AC_MSG_WARN(Not linking with curl for remote file i/o support)
fi

AC_ARG_ENABLE(
  reentrant,
  [AS_HELP_STRING([--enable-reentrant],[Enable reentrant multithreading])],
  [ if test $enableval = yes; then BUILD_REENTRANT=yes; fi ]
)

SSE_FLAGS=""
AC_ARG_ENABLE(
  sse2,
  [AS_HELP_STRING([--enable-sse2],[Enable use of instructions in the SSE2 extended instruction set])],
  [ if test $enableval = yes; then SSE_FLAGS="-msse2"; fi ]
)

AC_ARG_ENABLE(
  ssse3,
  [AS_HELP_STRING([--enable-ssse3],[Enable use of instructions in the SSSE3 extended instruction set])],
  [ if test $enableval = yes; then SSE_FLAGS="$SSE_FLAGS -mssse3"; fi ]
)

SYMBOLS=""
AC_ARG_ENABLE(
  symbols,
  [AS_HELP_STRING([--enable-symbols],[Enable debugging symbols by turning optimization off])],
  [ if test $enableval = yes; then SYMBOLS=yes; fi ]
)

# Define BUILD_HERA when building for HERA project to activate code in
# drvrfile.c (by way of fitsio2.h):
AC_ARG_ENABLE(
  hera,
  [AS_HELP_STRING([--enable-hera],[Build for HERA (ASD use only)])],
  [ if test $enableval = yes; then BUILD_HERA=yes; fi ]
)
if test "x$BUILD_HERA" = xyes; then
  AC_DEFINE(BUILD_HERA)
fi

# Optional support for bzip2 compression:
AC_ARG_WITH(
  bzip2,
  [AS_HELP_STRING([--with-bzip2[[=PATH]]],[Enable bzip2 support. Optional path to the location of include/bzlib.h and lib/libbz2])],
  [ if test "x$withval" != "xno"; then
        if test "x$withval" = "xyes" ; then
            AC_CHECK_LIB([bz2],[main],[],[AC_MSG_ERROR(Unable to locate bz2 library needed when enabling bzip2 support; try specifying the path)])
        else
            BZIP2_PATH="${withval}"
        fi
        AC_CHECK_HEADERS(bzlib.h,[AC_DEFINE(HAVE_BZIP2,1,[Define if you want bzip2 read support])])
    fi
  ]
)

# Optional Globus Toolkit support:
AC_ARG_WITH(
  gsiftp,
  [AS_HELP_STRING([--with-gsiftp[[=PATH]]],[Enable Globus Toolkit gsiftp protocol support])],
  [ if test "x$withval" != "xno"; then
        if test "x$withval" != "xyes" ; then
            GSIFTP_PATH="${withval}"
        fi
        AC_DEFINE(HAVE_GSIFTP,1,[Define if you want Globus Toolkit gsiftp protocol support])
        USE_GSIFTP=yes
    fi
  ]
)
AM_CONDITIONAL(NOGSIFTP, test x$USE_GSIFTP != xyes)

AC_ARG_WITH(
  gsiftp-flavour,
  [AS_HELP_STRING([--with-gsiftp-flavour[[=PATH]]],[Define Globus Toolkit gsiftp protocol flavour])],
  [ if test "x$withval" != "xno"; then
        if test "x$withval" != "xyes" ; then
            GSIFTP_FLAVOUR="${withval}"
        fi
        AC_DEFINE(GSIFTP_FLAVOUR,1,[Define Globus Toolkit architecture])
    fi
  ]
)

# Optionally disable check for zlib:
ZLIB_CHECK=yes
AC_ARG_WITH(
  zlib-check,
  [AS_HELP_STRING([--without-zlib-check],[Disable check for zlib compression library e.g. for cross-compilers])],
  [ if test "x$withval" = "xno"; then
       AC_MSG_WARN(cfitsio: ZLIB is required!)
       ZLIB_CHECK=no
    fi
  ]
)

# Optionally disable Fortran wrappers:
use_fortran=yes
AC_ARG_WITH([fortran],
  [AS_HELP_STRING([--without-fortran],[Build package without Fortran support])],
  [ if test "x$withval" = "xno"; then
       AC_MSG_WARN(cfitsio: Building without Fortran support)
       use_fortran=no
    fi
  ]
)

#--------------------------------------------------------------------
# Check for install location prefix
#--------------------------------------------------------------------

AC_PREFIX_DEFAULT(`pwd`)

# make will complain about duplicate targets for the install directories
# if prefix == exec_prefix
AC_SUBST(INSTALL_ROOT,'${prefix}')
test "$exec_prefix" != NONE -a "$prefix" != "$exec_prefix" \
  && INSTALL_ROOT="$INSTALL_ROOT "'${exec_prefix}'

#-------------------------------------------------------------------------------
# Handle --enable-symbols=yes
#-------------------------------------------------------------------------------

# Remove existing/default optimization:
if test "x$SYMBOLS" = "xyes"; then
   changequote(,)
   COPT_DEFAULT=`echo $CFLAGS | tr " " "\012" | grep "^-O[0-9]"`
   changequote([,])
   CFLAGS=`echo $CFLAGS | sed "s:$COPT_DEFAULT::g"`
fi

#-------------------------------------------------------------------------------

# Fortran wrappers:
# -----------------

# Use GNU Fortran wrappers by default:

if test "x$use_fortran" = "xyes"; then
  AC_MSG_NOTICE(cfitsio: == Adding Fortran wrappers)
  #AC_PROG_FC
  if test "x$FC" != "x" ; then F77=$FC; fi
  AC_PROG_F77
  #AC_CHECK_PROGS(FC, gfortran g95 g77 f77 ifort f95 f90 xlf cf77 gf77 af77 ncf f2c, notfound)
  if test "x$F77" != "x" ; then
     #FC_BASENAME=`basename $FC`
     FC_BASENAME=`basename $F77`
     if test "x$FC_BASENAME" = "xf2c" ; then
        AC_MSG_NOTICE(cfitsio: == Adding wrapper support for f2c)
        CFORTRANFLAGS="-Df2cFortran"
     fi
  else
     use_fortran=no
     AC_MSG_NOTICE(cfitsio: == Adding wrapper support for GNU Fortran by default)
     CFORTRANFLAGS="-Dg77Fortran"
  fi
  #if test "x$GFC" = "xyes" ; then
  if test "x$G77" = "xyes" ; then
     AC_MSG_NOTICE(cfitsio: == Adding wrapper support for GNU Fortran)
     CFORTRANFLAGS="-Dg77Fortran"
  fi
else
  CFORTRANFLAGS=
fi
AM_CONDITIONAL(NOFORTRAN, test x$use_fortran = xno)

# ar & ranlib required
#---------------------
AC_CHECK_PROG(AR, ar, ar, noar)
if test $AR = noar; then
  AC_MSG_ERROR(ar not found in your \$PATH. See your sysdamin.)
fi
ARCHIVE="$AR rv"
AC_SUBST(ARCHIVE)

dnl Checks for ANSI stdlib.h.
AC_CHECK_HEADERS(stdlib.h string.h math.h limits.h ,ANSI_HEADER=yes,ANSI_HEADER=no)dnl

dnl Check if prototyping is allowed.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[void d( int , double) ]])],[PROTO=yes],[PROTO=no])dnl

if test $ANSI_HEADER = no -o $PROTO = no; then
     echo "   *********** WARNING: CFITSIO CONFIGURE FAILURE ************  "
     echo "cfitsio: ANSI C environment NOT found. Aborting cfitsio configure."
     if test $ANSI_HEADER = no; then 
        echo "cfitsio: You're missing a needed ANSI header file."
     fi
     if test $PROTO = no; then
        echo "cfitsio: Your compiler can't do ANSI function prototypes."
     fi
     echo "cfitsio: You need an ANSI C compiler and all ANSI trappings"
     echo "cfitsio: to build cfitsio.   "
     echo "     *******************************************************  "
     exit 0;
fi

dnl Check if C compiler supports sse extended instruction flags.
if test "x$SSE_FLAGS" != x; then
    SAVE_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $SSE_FLAGS"
    AC_MSG_CHECKING([whether $CC accepts $SSE_FLAGS])
    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],[c_has_option=yes],[c_has_option=no])
    AC_MSG_RESULT($c_has_option)
    AC_LANG_POP([])
    if test "$c_has_option" = no; then SSE_FLAGS=""; fi
    CFLAGS="$SAVE_CFLAGS"
fi
AC_SUBST(SSE_FLAGS)

CFLAGS="$CFLAGS"
LIBPRE=""

case $host in
    *cygwin*)
	CFLAGS="$CFLAGS -DHAVE_POSIX_SIGNALS"
	;;
    *apple-darwin*)
        # For large file support (but may break Absoft compilers):
        AC_DEFINE(_LARGEFILE_SOURCE)
        AC_DEFINE(_FILE_OFFSET_BITS,64)
	;;
    *haiku*)
        # For large file support:
        AC_DEFINE(_LARGEFILE_SOURCE)
        AC_DEFINE(_FILE_OFFSET_BITS,64)
        ;;
    *hpux*)
	if test "x$CFORTRANFLAGS" = x ; then
		CFORTRANFLAGS="-Dappendus"
	fi
	CFLAGS="$CFLAGS -DPG_PPU" 
	LIBPRE="-Wl,"
	;;
    *irix*)
	CFLAGS="$CFLAGS -DHAVE_POSIX_SIGNALS"
	RANLIB="touch"
	;;
    *linux*)
        # For large file support:
        AC_DEFINE(_LARGEFILE_SOURCE)
        AC_DEFINE(_FILE_OFFSET_BITS,64)
	;;
    *mingw32*)
        AC_MSG_CHECKING([for large file support])
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <stdio.h>],
            [_FILE_OFFSET_BITS_SET_FSEEKO])],[
            AC_DEFINE(_LARGEFILE_SOURCE)
            AC_DEFINE(_FILE_OFFSET_BITS,64)
            AC_MSG_RESULT(yes)
        ],[AC_MSG_RESULT(no)])
        ;;
    *solaris*)
        if test "x$CFORTRANFLAGS" = x ; then
                CFORTRANFLAGS="-Dsolaris"
        fi
        # We need libm on Solaris:
        AC_CHECK_LIB(m, frexp)
        # For large file support:
        AC_DEFINE(_LARGEFILE_SOURCE)
        AC_DEFINE(_FILE_OFFSET_BITS,64)
	;;
    *)
		echo "cfitsio: == Don't know what do do with $host"
	;;
esac

CFLAGS="$CFLAGS $CFORTRANFLAGS"

case $GCC in
	yes)
		GCCVERSION="`$CC -dumpversion 2>&1`"
		echo "cfitsio: == Using $CC version $GCCVERSION"
		AC_SUBST(GCCVERSION)
                changequote(,)
		gcc_test=`echo $GCCVERSION | grep -c '2\.[45678]'`
		changequote([,])
		if test $gcc_test -gt 0
		then
		    changequote(,)
		    CFLAGS=`echo $CFLAGS | sed 's:-O[^ ]* *::'`
		    changequote([,])
		    AC_MSG_WARN(This gcc is pretty old. Disabling optimization to be safe.)
		fi
		;;
	no)
		echo "cfitsio: Old CFLAGS is $CFLAGS"
		CFLAGS=`echo $CFLAGS | sed -e "s/-g/-O/"`
		case $host in
		    *solaris*)
			changequote(,)
			if test `echo $CFLAGS | grep -c fast` -gt 0
			then
			   echo "cfitsio: Replacing -fast with -O3"
			   CFLAGS=`echo $CFLAGS | sed 's:-fast:-O3:'`
			fi
			changequote([,])
         		CFLAGS="$CFLAGS -DHAVE_ALLOCA_H -DHAVE_POSIX_SIGNALS"
			;;
		    *)
			echo "== No special changes for $host"
			;;
		esac
		echo "New CFLAGS is $CFLAGS"
		;;
	*)
		# Don't do anything now
		;;
esac

# Curl library (will be pulled in to the shared CFITSIO library):
# ---------------------------------------------------------------
CURL_INC=""
CURL_LIB=""
CURL_LIB_PATH=""
if test "x$ADD_CURL" = xyes; then
  # Use curl-config to get compiler & linker flags, if available.
  # (and prefer /usr/bin/curl-config)
  AC_PATH_PROG([CURLCONFIG], [curl-config], [], [/usr/bin$PATH_SEPARATOR$PATH], [])
  if test "x$CURLCONFIG" != x; then
    CURL_LIB=`$CURLCONFIG --libs`
    CURL_INC=`$CURLCONFIG --cflags`
    if test "x$CURL_LIB" != x; then
      LIBS_CURL="$CURL_LIB"
      # Mac OS: For third-party curl-config, acquire an rpath:
      if test `echo $host | grep -c apple-darwin` -ne 0 -a `echo $CURL_LIB | grep -c "^-L"` -gt 0; then
        CURL_LIB_PATH=`echo ${CURL_LIB} | tr " " "\012" | grep "^-L" | tr "\012" " " | sed 's:-L::' | sed 's:[ ]$::'`
        if test "x$CURL_LIB_PATH" != x; then
          LIBS_CURL="-Wl,-rpath,$CURL_LIB_PATH $CURL_LIB"
        fi
      fi
      if test `echo $host | grep -c cygwin` -ne 0 -o `echo $host | grep -c mingw32` -ne 0; then
          LIBS="$CURL_LIB $LIBS"
      fi
      AC_DEFINE(CFITSIO_HAVE_CURL)
    fi
    if test "x$CURL_INC" != x; then
      CFLAGS="$CURL_INC $CFLAGS"
    fi
  # No curl-config:
  else
    AC_MSG_WARN(curl-config not found. Disabling curl support.)
  # Incomplete stubs for possible future use:
  #  AC_CHECK_LIB([curl],[main],[],
  #       [AC_MSG_WARN(Not building curl support for CFITSIO)])
  #  AC_CHECK_HEADER(curl.h,[])
  fi
fi
AC_SUBST(LIBS_CURL)

# ZLIB (required):
if test "x$ZLIB_CHECK" = xyes; then
   AC_CHECK_LIB([z], [inflateEnd], [],[AC_MSG_ERROR(Unable to locate zlib compression library)])
fi

# GSIFTP flags:
if test "x$GSIFTP_PATH" != x -a "x$GSIFTP_FLAVOUR" != x; then
   CFLAGS="$CFLAGS -I${GSIFTP_PATH}/include/${GSIFTP_FLAVOUR}"
   LIBS="$LIBS -L${GSIFTP_PATH}/lib -lglobus_ftp_client_${GSIFTP_FLAVOUR}"
fi

# BZIP2 flags:
if test "x$BZIP2_PATH" != x; then
   CFLAGS="$CFLAGS -I${BZIP2_PATH}/include"
   LIBS="$LIBS -L${BZIP2_PATH}/lib -lbz2"
fi

AC_SUBST(ARCH)dnl
AC_SUBST(CFLAGS)dnl
AC_SUBST(CC)dnl
AC_SUBST(FC)dnl
AC_SUBST(LIBPRE)dnl

# ================= test for the unix ftruncate function ================

AC_MSG_CHECKING(whether ftruncate works)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <unistd.h>
]], [[
ftruncate(0, 0);
]])],[
AC_DEFINE(HAVE_FTRUNCATE)
AC_MSG_RESULT(yes)
],[AC_MSG_RESULT(no) ])

# ---------------------------------------------------------
# some systems define long long for 64-bit ints
# ---------------------------------------------------------
 
AC_MSG_CHECKING(whether long long is defined)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdlib.h>
]], [[
long long filler;
]])],[
AC_DEFINE(HAVE_LONGLONG)
AC_MSG_RESULT(yes)
],[AC_MSG_RESULT(no) ])

# ==================== SHARED MEMORY DRIVER SECTION =======================
#
# 09-Mar-98 : modified by JB/ISDC
# 3 checks added to support autoconfiguration of shared memory
# driver. First generic check is made whether shared memory is supported
# at all, then 2 more specific checks are made (architecture dependent).
# Currently tested on : sparc-solaris, intel-linux, sgi-irix, dec-alpha-osf

# -------------------------------------------------------------------------
# check is System V IPC is supported on this machine
# -------------------------------------------------------------------------

AC_MSG_CHECKING(whether system V style IPC services are supported)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
]], [[
shmat(0, 0, 0);
shmdt(0);
shmget(0, 0, 0);
semget(0, 0, 0);
]])],[
AC_DEFINE(HAVE_SHMEM_SERVICES)
my_shmem=\${SOURCES_SHMEM}
AC_MSG_RESULT(yes)
],[AC_MSG_RESULT(no) ])

AC_SUBST(my_shmem)

# -------------------------------------------------------------------------
# some systems define flock_t, for others we have to define it ourselves
# -------------------------------------------------------------------------

AC_MSG_CHECKING(whether flock_t is defined in sys/fcntl.h)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/fcntl.h>
]], [[
flock_t filler;
]])],[
AC_DEFINE(HAVE_FLOCK_T)
AC_MSG_RESULT(yes) 
],[AC_MSG_RESULT(no) ])

if test "$HAVE_FLOCK_T" != 1; then
   AC_MSG_CHECKING(whether flock_t is defined in sys/flock.h)
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/flock.h>
   ]], [[
   flock_t filler;
   ]])],[
   AC_DEFINE(HAVE_FLOCK_T)
   AC_MSG_RESULT(yes) 
   ],[AC_MSG_RESULT(no) ])
fi

# ------------------------------------------------------------------------------
# Define _REENTRANT & add -lpthread to LIBS if reentrant multithreading enabled:
# ------------------------------------------------------------------------------
if test "x$BUILD_REENTRANT" = xyes; then
  AC_DEFINE(_REENTRANT)
  AC_DEFINE([_XOPEN_SOURCE], [700])
  # Additional definition needed to get 'union semun' when using
  # _XOPEN_SOURCE on a Mac:
  if test "x$EXT" = xdarwin; then
    AC_DEFINE([_DARWIN_C_SOURCE])
  fi
  AC_CHECK_LIB([pthread],[main],[],[AC_MSG_ERROR(Unable to locate pthread library needed when enabling reentrant multithreading)])
fi

# -------------------------------------------------------------------------
# there are some idiosyncrasies with semun defs (used in semxxx). Solaris
# does not define it at all
# -------------------------------------------------------------------------

AC_MSG_CHECKING(whether union semun is defined)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
]], [[
union semun filler;
]])],[
AC_DEFINE(HAVE_UNION_SEMUN)   
AC_MSG_RESULT(yes)
],[AC_MSG_RESULT(no) ])

# -------------------------------------------------------------------------
# fmemopen is not available on e.g. older Macs:
# -------------------------------------------------------------------------

AC_CHECK_FUNC(fmemopen, AC_DEFINE(HAVE_FMEMOPEN), [AC_MSG_WARN(Disabling support for compressed files via FTPS)])

# ==================== END OF SHARED MEMORY DRIVER SECTION ================
# ================= test for the unix networking functions ================

AC_SEARCH_LIBS([gethostbyname], [nsl network], cfitsio_have_nsl=1, cfitsio_have_nsl=0)
AC_SEARCH_LIBS([connect], [socket network], cfitsio_have_socket=1,
               cfitsio_have_socket=0, [-lnsl])

if test "$cfitsio_have_nsl" = 1 -a "$cfitsio_have_socket" = 1; then
   AC_DEFINE(HAVE_NET_SERVICES)
fi

# ==================== END OF unix networking SECTION ================

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
AC_CONFIG_FILES([cfitsio.pc])
AC_OUTPUT


AC_MSG_RESULT([])
AC_MSG_RESULT([    Congratulations, Makefile update was successful.])
AC_MSG_RESULT([    You may want to run make now.])
AC_MSG_RESULT([])
