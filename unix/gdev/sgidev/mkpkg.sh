# Make the SGI translators and install them in hlib.

$CC -c $HSI_CF	sgidispatch.c
$CC $HSI_CF	sgidispatch.o ../../hlib/libos.a -o sgidispatch.E
mv -f		sgidispatch.E ../../hlib
rm		sgidispatch.o

$CC -c $HSI_CF	sgi2uimp.c
$CC $HSI_CF	sgi2uimp.o -o sgi2uimp.E
mv -f		sgi2uimp.E ../../hlib
rm		sgi2uimp.o

$CC -c $HSI_CF	sgi2uapl.c
$CC $HSI_CF	sgi2uapl.o -o sgi2uapl.E
mv -f		sgi2uapl.E ../../hlib
rm		sgi2uapl.o

$CC -c $HSI_CF	sgi2uqms.c
$CC $HSI_CF	sgi2uqms.o -o sgi2uqms.E
mv -f		sgi2uqms.E ../../hlib
rm		sgi2uqms.o

$CC -c $HSI_CF	sgi2uptx.c
$CC $HSI_CF	sgi2uptx.o -o sgi2uptx.E
mv -f		sgi2uptx.E ../../hlib
rm		sgi2uptx.o

$CC -c $HSI_CF	sgi2uhplj.c
$CC $HSI_CF	sgi2uhplj.o -o sgi2uhplj.E
mv -f		sgi2uhplj.E ../../hlib
rm		sgi2uhplj.o

$CC -c $HSI_CF	sgi2uhpgl.c
$CC $HSI_CF	sgi2uhpgl.o -o sgi2uhpgl.E
mv -f		sgi2uhpgl.E ../../hlib
rm		sgi2uhpgl.o

$CC -c $HSI_CF	sgi2ueps.c
$CC $HSI_CF	sgi2ueps.o -o sgi2ueps.E
mv -f		sgi2ueps.E ../../hlib
rm		sgi2ueps.o
