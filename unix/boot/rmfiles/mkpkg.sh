# Make and install the RMFILES utility.

cc -c $HSI_CF	rmfiles.c
cc $HSI_CF	rmfiles.o $HSI_LIBS -o rmfiles.e
mv -f		rmfiles.e ../../hlib
rm		*.o
