# Make the XC driver program.

$CC -c $HSI_CF xc.c
$CC $HSI_LF xc.o $HSI_LIBS -o xc.e
mv -f	xc.e ../../hlib
rm xc.o
