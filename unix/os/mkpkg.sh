# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"
cc $HSI_CF	alloc.c getproc.c -o alloc.e
chmod		4755 alloc.e
mv -f		alloc.e ../hlib
rm		alloc.o

cc -c $HSI_CF	../as/zsvjmp.s [b-z]*.c
ar rv		libos.a *.o; ar dv libos.a zmain.o; rm *.o
ranlib		libos.a
mv -f		libos.a ../bin
