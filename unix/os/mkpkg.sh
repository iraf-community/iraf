# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"


zsvjmp=../as/zsvjmp.s
$CC -c $HSI_CF -Wall $zsvjmp gmttolst.c irafpath.c prwait.c z*.c alloc.c getproc.c
$CC $HSI_LF -Wall alloc.o getproc.o $HSI_OSLIBS -o alloc.e
chmod	4755 alloc.e
mv -f	alloc.e ../hlib
rm -f	alloc.o zmain.o
ar r	libos.a *.o; 
ranlib	libos.a

rm *.o
mv -f		libos.a ../bin
