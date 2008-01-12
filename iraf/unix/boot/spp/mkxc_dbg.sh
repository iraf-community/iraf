# Make the XC driver program.

$CC -c -g $HSI_CF xc.c
$CC $HSI_LF -g xc.o $HSI_LIBS -o xc.e
mv -f	xc.e ../../bin.redhat
rm xc.o
