#!/bin/sh
# Make the Subset Preprocessor language (SPP) compiler.

# Exit on error
set -e

echo "----------------------- XC  ----------------------------"
$CC -c $HSI_CF	xc.c
$CC $HSI_LF	xc.o $HSI_LIBS -o xc.e
mv -f		xc.e ../../hlib
rm -f		xc.o

echo "----------------------- XPP ----------------------------"
(cd xpp; sh -x mkpkg.sh)
echo "----------------------- RPP ----------------------------"
(cd rpp; sh -x mkpkg.sh)
