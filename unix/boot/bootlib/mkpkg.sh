# Make the bootstrap utilities library (bootlib).

if test -f ../../as/bytmov.s; then\
    $CC -c $HSI_CF ../../as/bytmov.s -o bytmov.o;\
else\
    $CC -c $HSI_CF _bytmov.c;\
fi

# $CC -c $HSI_CF [a-z]*.c
for i in [a-z]*.c ;\
do	$CC -c $HSI_CF $i ;\
done

ar rv		libboot.a *.o; rm *.o
$RANLIB		libboot.a
mv -f		libboot.a ../../bin
