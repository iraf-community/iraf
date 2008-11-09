# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"

if test "$IRAFARCH" = "macosx"; then
    export larch="-arch ppc"
elif test "$IRAFARCH" = "macintel"; then
    export larch="-arch i386"
else
    set larch=""
fi

$CC -c $HSI_CF $larch alloc.c getproc.c
$CC $HSI_LF $larch alloc.o getproc.o $HSI_OSLIBS -o alloc.e
chmod		4755 alloc.e
mv -f		alloc.e ../hlib
rm -f		alloc.o

for i in zsvjmp ;\
    do $CC -c $HSI_CF $larch ../as/$i.s -o $i.o ;\
done
for i in gmttolst.c irafpath.c prwait.c z*.c ;\
    do $CC -c $HSI_CF $larch $i ;\
done

ar rv		libos.a *.o; ar dv libos.a zmain.o; rm *.o
$RANLIB		libos.a
mv -f		libos.a ../bin
