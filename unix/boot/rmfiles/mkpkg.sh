# Make and install the RMFILES utility.

$CC -c $HSI_CF	rmfiles.c
$CC $HSI_CF	rmfiles.o $HSI_LIBS -o rmfiles.E
mv -f		rmfiles.E ../../hlib
rm		*.o
