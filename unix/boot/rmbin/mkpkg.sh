# Make and install the RMBIN utility.

cc -c $HSI_CF	rmbin.c
cc $HSI_CF	rmbin.o $HSI_LIBS -o rmbin.e
mv -f		rmbin.e ../../hlib
rm		*.o
