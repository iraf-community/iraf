# Bootstrap RTAR.

$CC -c $HSI_CF	rtar.c
$CC $HSI_LF	rtar.o $HSI_LIBS -o rtar.e
mv		rtar.e ../../hlib
rm -f		rtar.o
