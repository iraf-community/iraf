# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"
$CC -c $HSI_CF alloc.c getproc.c
$CC $HSI_LF alloc.o getproc.o $HSI_OSLIBS -o alloc.e
chmod		4755 alloc.e
mv -f		alloc.e ../hlib
rm -f		alloc.o

zsrc=`ls z*.c | grep -v zshlib`
for i in ../as/zsvjmp.s ../as/zshlib.s gmttolst.c irafpath.c prwait.c $zsrc ;\
do	$CC -c $HSI_CF $i ;\
done

ar rv		libos.a *.o; ar dv libos.a zmain.o; rm *.o
$RANLIB		libos.a
mv -f		libos.a ../bin
