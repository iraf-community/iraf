# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"


$CC -c $HSI_CF -Wall alloc.c getproc.c
$CC $HSI_LF -Wall alloc.o getproc.o $HSI_OSLIBS -o alloc.e
chmod		4755 alloc.e
mv -f		alloc.e ../hlib
rm -f		alloc.o


if test "$IRAFARCH" != "macosx"; then
    for i in zsvjmp ;\
        do $CC -c $HSI_CF -Wall ../as/$i.s -o $i.o ;\
    done
fi


for i in gmttolst.c irafpath.c prwait.c z*.c ;\
    do $CC -c $HSI_CF -Wall $i ;\
done

#ar rv		libos.a *.o; ar dv libos.a zmain.o; rm *.o

if [ "$IRAFARCH" = "macosx" ]; then
##    $CC -c -O -DMACOSX -w -Wunused -arch ppc  ../as/zsvjmp_ppc.s  -o zsvjmp.o ;\
##    libtool -a -T -o libos.a zsvjmp.o
##    rm -f zsvjmp.o
    $CC -c -O -DMACOSX -w -Wunused -m32 -arch i386 ../as/zsvjmp_i386.s -o zsvjmp.o ;\
    ar r	libos.a *.o; 
    ranlib	libos.a
    rm -f 	zsvjmp.o zmain.o

else
    rm -f zmain.o
    ar r	libos.a *.o; 
    ranlib	libos.a
fi

rm *.o
mv -f		libos.a ../bin
