# Make the XC driver program.

cc $HSI_CF xc.c $HSI_LIBS -o xc.e
mv -f	xc.e ../../hlib
