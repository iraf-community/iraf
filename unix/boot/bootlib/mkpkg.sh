# Make the bootstrap utilities library (bootlib).

if test -f ../../as/bytmov.s; then\
    cc -c $HSI_CF ../../as/bytmov.s -o bytmov.o;\
else\
    cc -c $HSI_CF _bytmov.c;\
fi

cc -c $HSI_CF	[a-z]*.c
ar rv		libboot.a *.o; rm *.o
ranlib		libboot.a
mv -f		libboot.a ../../bin
