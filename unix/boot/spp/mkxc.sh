# Make the XC driver program.

$CC $HSI_CF xc.c $HSI_LIBS -o xc.E
mv -f	xc.E ../../hlib
