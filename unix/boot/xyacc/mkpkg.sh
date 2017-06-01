# XYACC -- Yacc parser generator for SPP.

$CC -c $HSI_CF	y[1-4].c
$CC $HSI_LF	y[1-4].o $HSI_LIBS -o xyacc.e
mv -f		xyacc.e ../../hlib
cp 		yaccpar.x ../../../lib
rm -f		*.o
