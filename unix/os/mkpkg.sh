# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"

# Initialize the $iraf and environment.
if test -f $hlib/envinit.sh ; then
    source $hlib/envinit.sh
fi

$CC -c $HSI_CF -Wall alloc.c getproc.c
$CC $HSI_LF -Wall alloc.o getproc.o $HSI_OSLIBS -o alloc.e
chmod		4755 alloc.e
mv -f		alloc.e ../hlib
rm -f		alloc.o


if [ "$IRAFARCH" = "macosx" -o "$IRAFARCH" = "macintel" ]; then
    mflags="-Wno-cast-function-type-mismatch -Wall "
    if [ "$IRAFARCH" = "macosx" ]; then
        mflags="${mflags} -DUSE_SSL -I/opt/homebrew/opt/openssl@3/include"
    else 
        mflags="${mflags} -DUSE_SSL -I/usr/local/opt/openssl@3/include"
    fi

    for i in gmttolst.c irafpath.c prwait.c  z[a-lo-z]*.c zmaloc.c zmfree.c ;\
        do $CC -c $HSI_CF ${mflags} $i ;\
    done

    if [ "$IRAFARCH" = "macosx" ]; then
        $CC -g -c -O -DMACOSX -Wall -m64 -arch arm64 ../as/zsvjmp.s -o zsvjmp.o
    else
        $CC -g -c -O -m64 -arch x86_64 ../as/zsvjmp.s -o zsvjmp.o
    fi
    ar r	libos.a *.o; 
    ranlib	libos.a
    #rm -f 	zsvjmp.o zmain.o

else
    for i in gmttolst.c irafpath.c prwait.c  z[a-lo-z]*.c zmaloc.c zmfree.c ;\
        do $CC -c $HSI_CF -Wall $i ;\
    done

    if [ "$IRAFARCH" = "linux64" ]; then
        $CC -c $HSI_CF -Wall -m64 ../as/zsvjmp.s -o zsvjmp.o
    else
        $CC -c $HSI_CF -Wall -m32 ../as/zsvjmp.s -o zsvjmp.o
    fi

    rm -f       zmain.o
    ar r	libos.a *.o; 
    ranlib	libos.a
fi

rm *.o
mv -f		libos.a ../bin
