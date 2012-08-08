# VOClient configuration file for Autoconf.
# Process this file with autoconf to create configure.

AC_PREREQ(2.59)

AC_INIT([voclient],
    [m4_normalize(esyscmd([cat VERSION]))],
    [http://www.us-vo.org])

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
AC_CONFIG_HEADERS([config/config.h:config/config-h.in])
AM_INIT_AUTOMAKE([1.9 foreign])

AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_SWIG(1.3.21)
#AM_PATH_PYTHON(2.3)
#SWIG_ENABLE_CXX
#SWIG_MULTI_MODULE_SUPPORT
#SWIG_PYTHON(use-shadow-classes=yes)

AC_CHECK_HEADERS([float.h])
AC_CHECK_HEADERS([limits.h])
AC_CHECK_HEADERS([math.h])
AC_CHECK_HEADERS([netdb.h])
AC_CHECK_HEADERS([stdarg.h])
AC_CHECK_HEADERS([stddef.h])
AC_CHECK_HEADERS([stdlib.h])
AC_CHECK_HEADERS([string.h])

AC_CHECK_HEADERS([php.h])
AC_CHECK_HEADERS([perl.h])
AC_CHECK_HEADERS([Python.h])
AC_CHECK_HEADERS([ruby.h])
AC_CHECK_HEADERS([jni.h])
AC_CHECK_HEADERS([tcl.h])
AC_CHECK_HEADERS([zend.h])

##AC_CHECK_FUNCS([foo])
##AC_REPLACE_FUNCS([foo])

AC_CONFIG_FILES([Makefile clientapi/Makefile clientapi/examples/Makefile])
AC_OUTPUT
