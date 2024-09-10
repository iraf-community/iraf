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

# This just gets CORE_SOURCES from Makefile.in
lib_files=$(make -f cfitsio/Makefile.in cfitsioLibSrcs | sed 's/zlib\/.*//')
flib_files='f77_wrap1.c f77_wrap2.c f77_wrap3.c f77_wrap4.c'

# The include files cannot be directly inferred from Makefile.in
inc_files='fitsio.h fitsio2.h longnam.h drvrsmem.h eval_defs.h eval_tab.h region.h group.h simplerng.h grparser.h f77_wrap.h cfortran.h'

extra_files='config.sub config.guess Makefile.in cfitsio.pc.in'

if [ ! -d cfitsio/lib ]; then
    mkdir cfitsio/lib
fi

for fil in $lib_files $flib_files $inc_files $extra_files; do
    if [ -f "cfitsio/$fil" ]; then
        mv "cfitsio/$fil" cfitsio/lib/
    fi
done

rm -rf cfitsio/cfitsio.xcodeproj
rm -rf cfitsio/docs
rm -f cfitsio/[!L]*.*
rm -f cfitsio/f77_* cfitsio/cfortran.h
mv cfitsio/lib/* cfitsio/
rmdir cfitsio/lib/

cat <<EOF >cfitsio/README.IRAF
Note: IRAF only requires the CFITSIO library, and hence in this bundled version,
we removed all other files except the required license (License.txt) and changelog
(docs/changes.txt, which has the version number).
EOF
