# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"
$CC $HSI_CF	alloc.c getproc.c -o alloc.E
chmod		4755 alloc.E
mv -f		alloc.E ../hlib
rm -f		alloc.o

# $CC -c $HSI_CF ../as/zsvjmp.s gmttolst.c irafpath.c prwait.c z*.c
for i in ../as/zsvjmp.s gmttolst.c irafpath.c prwait.c z*.c ;\
do	$CC -c $HSI_CF $i ;\
done

ar rv		libos.a *.o; ar dv libos.a zmain.o; rm *.o
$RANLIB		libos.a
mv -f		libos.a ../bin
