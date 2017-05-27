# Make the first pass (XPP) of the SPP language compiler.

if [ ! -f lexyy.c -o xpp.l -nt lexyy.c ] ; then
    lex	-o lexyy.c -t xpp.l
fi

$CC -c $HSI_CF	xppmain.c xppcode.c decl.c lexyy.c
$CC $HSI_LF	xppmain.o lexyy.o xppcode.o decl.o $HSI_LIBS -o xpp.e
mv -f		xpp.e ../../../hlib
rm		*.o
