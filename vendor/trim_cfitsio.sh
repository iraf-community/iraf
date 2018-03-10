#!/bin/sh
# Copyright (c) 2011-2017, Astropy Developers
# Distributable under a BSD-3-Clause license

# This script is adopted from astropy and adjusted for the needs in IRFA.
# It should be run every time cfitsio is updated.
# This moves all the code needed for the actual library to lib
# and deletes everything else (except License.txt and doc/changes.txt)

# So, the standard update would be to execute, from this directory,
# rm -rf cfitsio
# tar xvf <PATH_TO_TAR>   # (e.g., cfitsio3410.tar.gz)
# ./trim_cfitsio.sh

set -e

# Locally patch wcsutil.c
patch -s -p1 <<EOF
--- b/cfitsio/wcsutil.c
+++ a/cfitsio/wcsutil.c
@@ -81,8 +81,9 @@ int ffwldp(double xpix, double ypix, double xref, double yref,
       dect = dec0 + m;
 
     } else if (*cptr == 'T') {  /* -TAN */
-      if (*(cptr + 1) != 'A' ||  *(cptr + 2) != 'N') {
-         return(*status = 504);
+      if ( !(*(cptr + 1) == 'A' && *(cptr + 2) == 'N') && 
+           !(*(cptr + 1) == 'P' && *(cptr + 2) == 'V') ) {
+         	return(*status = 504);
       }
       x = cos0*cos(ra0) - l*sin(ra0) - m*cos(ra0)*sin0;
       y = cos0*sin(ra0) + l*cos(ra0) - m*sin(ra0)*sin0;

EOF

# This just gets CORE_SOURCES from Makefile.in
lib_files=`make -f cfitsio/Makefile.in cfitsioLibSrcs | sed 's/zlib\/.*//'`
flib_files='f77_wrap1.c f77_wrap2.c f77_wrap3.c f77_wrap4.c'
# The include files cannot be directly inferred from Makefile.in
inc_files='fitsio.h fitsio2.h longnam.h drvrsmem.h eval_defs.h eval_tab.h region.h group.h simplerng.h grparser.h f77_wrap.h cfortran.h'
extra_files='config.sub config.guess Makefile.in'

if [ ! -d cfitsio/lib ]; then
    mkdir cfitsio/lib
fi

for fil in $lib_files $flib_files $inc_files $extra_files; do
    if [ -f cfitsio/$fil ]; then
        mv cfitsio/$fil cfitsio/lib/
    fi
done
rm -rf cfitsio/cfitsio.xcodeproj
rm -rf cfitsio/docs
rm -f cfitsio/[^L]*.*
mv cfitsio/lib/* cfitsio/
rmdir cfitsio/lib/
cat <<EOF >cfitsio/README.IRAF
Note: IRAF only requires the CFITSIO library, and hence in this bundled version,
we removed all other files except the required license (License.txt) and changelog
(docs/changes.txt, which has the version number).
EOF
cat <<EOF >cfitsio/mklibs
#!/bin/sh

top=\$(pwd)

export CC=gcc
export CXX=g++


if [ "\$PLMACH" = "macosx" ] ; then
   export CFLAGS="-DDarwin"
fi

echo "  (Using toplevel directory \$top ....)"

# Global options.
gopts="--prefix=\$top --exec-prefix=\$top --disable-shared"

./configure \$gopts  			       | tee _spool 2>&1
make clean 				       | tee -a _spool 2>&1
make 					       | tee -a _spool 2>&1

cp   libcfitsio.a ../voclient/lib
mv   libcfitsio.a ../../lib
cp   fitsio*.h longnam.h ../../include

make clean
echo "done"
EOF

chmod +x cfitsio/mklibs
