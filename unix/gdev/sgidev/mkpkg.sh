#!/bin/sh
# Make the SGI translators and install them in hlib.

# Exit on error
set -e

$CC -c $HSI_CF	sgiUtil.c

$CC -c $HSI_CF	sgidispatch.c
$CC $HSI_LF	sgidispatch.o ../../hlib/libos.a $HSI_LIBS -o sgidispatch.e
mv -f		sgidispatch.e ../../hlib
rm		sgidispatch.o

$CC -c $HSI_CF	sgi2uimp.c
$CC $HSI_LF	sgi2uimp.o sgiUtil.o $HSI_LIBS -o sgi2uimp.e
mv -f		sgi2uimp.e ../../hlib
rm		sgi2uimp.o

$CC -c $HSI_CF	sgi2uapl.c
$CC $HSI_LF	sgi2uapl.o sgiUtil.o $HSI_LIBS -o sgi2uapl.e
mv -f		sgi2uapl.e ../../hlib
rm		sgi2uapl.o

$CC -c $HSI_CF	sgi2uqms.c
$CC $HSI_LF	sgi2uqms.o sgiUtil.o $HSI_LIBS -o sgi2uqms.e
mv -f		sgi2uqms.e ../../hlib
rm		sgi2uqms.o

$CC -c $HSI_CF	sgi2uptx.c
$CC $HSI_LF	sgi2uptx.o sgiUtil.o $HSI_LIBS -o sgi2uptx.e
mv -f		sgi2uptx.e ../../hlib
rm		sgi2uptx.o

$CC -c $HSI_CF	sgi2uhplj.c
$CC $HSI_LF	sgi2uhplj.o sgiUtil.o $HSI_LIBS -o sgi2uhplj.e
mv -f		sgi2uhplj.e ../../hlib
rm		sgi2uhplj.o

$CC -c $HSI_CF	sgi2uhpgl.c
$CC $HSI_LF	sgi2uhpgl.o sgiUtil.o $HSI_LIBS -o sgi2uhpgl.e
mv -f		sgi2uhpgl.e ../../hlib
rm		sgi2uhpgl.o

$CC -c $HSI_CF	sgi2ueps.c
$CC $HSI_LF	sgi2ueps.o sgiUtil.o $HSI_LIBS -o sgi2ueps.e
mv -f		sgi2ueps.e ../../hlib
rm		sgi2ueps.o

$CC -c $HSI_CF	sgi2gif.c
$CC $HSI_LF	sgi2gif.o sgiUtil.o $HSI_LIBS -o sgi2gif.e
mv -f		sgi2gif.e ../../hlib
rm		sgi2gif.o

$CC -c $HSI_CF	sgi2xbm.c
$CC $HSI_LF	sgi2xbm.o sgiUtil.o $HSI_LIBS -o sgi2xbm.e
mv -f		sgi2xbm.e ../../hlib
rm		sgi2xbm.o

$CC -c $HSI_CF	sgi2svg.c
$CC $HSI_LF	sgi2svg.o sgiUtil.o $HSI_LIBS -o sgi2svg.e
mv -f		sgi2svg.e ../../hlib
rm		sgi2svg.o

rm		sgiUtil.o
