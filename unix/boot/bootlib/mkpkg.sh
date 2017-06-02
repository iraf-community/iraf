# Make the bootstrap utilities library (bootlib).

# $CC -c $HSI_CF [a-z]*.c
for i in [a-z]*.c ;\
do	$CC -c $HSI_CF $i ;\
done

ar rv		libboot.a *.o; rm *.o
$RANLIB		libboot.a
mv -f		libboot.a ../../bin
