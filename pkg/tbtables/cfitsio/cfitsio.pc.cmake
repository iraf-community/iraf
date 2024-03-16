prefix=@CMAKE_INSTALL_PREFIX@
exec_prefix=@CMAKE_INSTALL_PREFIX@
libdir=@LIB_DESTINATION@
includedir=@INCLUDE_INSTALL_DIR@

Name: cfitsio
Description: FITS File Subroutine Library
URL: https://heasarc.gsfc.nasa.gov/fitsio/
Version: @CFITSIO_MAJOR@.@CFITSIO_MINOR@.@CFITSIO_MICRO@
Libs: -L${libdir} -lcfitsio
Libs.private: @PKG_CONFIG_LIBS@ -lm
Cflags: -I${includedir}
