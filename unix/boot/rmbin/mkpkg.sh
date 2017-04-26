# Make and install the RMBIN utility.

$CC -c $HSI_CF	rmbin.c
$CC $HSI_LF	rmbin.o $HSI_LIBS -o rmbin.e
mv -f		rmbin.e ../../hlib
rm		*.o
