# Make and install the RMBIN utility.

$CC -c $HSI_CF	rmbin.c
$CC $HSI_CF	rmbin.o $HSI_LIBS -o rmbin.E
mv -f		rmbin.E ../../hlib
rm		*.o
