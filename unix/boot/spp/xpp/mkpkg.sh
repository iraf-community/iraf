# Make the first pass (XPP) of the SPP language compiler.

find xpp.l -newer lexyy.c -exec rm lexyy.c \;
if test -f lexyy.c; then\
    $CC -c $HSI_CF -Wsign-compare lexyy.c;\
else\
    lex	-l xpp.l;\
    sed -f lex.sed lex.yy.c > lexyy.c;  rm lex.yy.c;\
    $CC -c $HSI_CF -Wsign-compare lexyy.c;\
fi

$CC -c $HSI_CF	xppmain.c xppcode.c decl.c
$CC $HSI_LF	xppmain.o lexyy.o xppcode.o decl.o $HSI_LIBS -o xpp.e

#echo "================================================="
#echo "Testing sample source conversion for differences:"
#echo "================================================="
#./xpp.e -R zztest.x
#diff -bitw zztest.r zztest.r.ref
#echo "================================================="
#echo "Done"
#echo "================================================="

mv -f		xpp.e ../../../hlib
rm -f		*.o zztest.r
