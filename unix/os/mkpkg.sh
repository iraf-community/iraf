# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"
$CC -c $HSI_CF alloc.c getproc.c
$CC $HSI_LF alloc.o getproc.o $HSI_OSLIBS -o alloc.e
chmod		4755 alloc.e
mv -f		alloc.e ../hlib
rm -f		alloc.o

#for i in zsvjmp enbint ;\
for i in zsvjmp ;\
    do $CC -c $HSI_CF ../as/$i.s -o $i.o ;\
done

for i in gmttolst.c irafpath.c prwait.c z*.c ;\
    do $CC -c $HSI_CF $i ;\
done

ar rv		libos.a *.o; ar dv libos.a zmain.o; rm *.o
$RANLIB		libos.a
mv -f		libos.a ../bin
