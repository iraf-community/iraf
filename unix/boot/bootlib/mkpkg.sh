# Make the bootstrap utilities library (bootlib).

if test -f ../../as/bytmov.s; then\
    cc -c $HSI_CF ../../as/bytmov.s -o bytmov.o;\
else\
    cc -c $HSI_CF _bytmov.c;\
fi

cc -c $HSI_CF	[a-z]*.c
case $MACH in
#Domain/OS: workaround for optimizer bug at sr10.2, cc 6.7.m:
    m68k) if test -f ${iraf}lib/libsys.a; then \
	      cc -W0,-opt,0,-nbss,-db -A nansi -c osstrupk.c; \
	  else \
	      cc -W0,-opt,0,-nbss,-db -A nansi -DNOVOS -c osstrupk.c; \
	  fi;;
esac

ar rv		libboot.a *.o; rm *.o
case $USE_RANLIB in
    yes)	ranlib libboot.a;;
esac
mv -f		libboot.a ../../bin
