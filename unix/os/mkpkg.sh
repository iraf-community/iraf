# Bootstrap the LIBOS.A library.

echo		"--------------------- OS ----------------------"

case $MACH in
#   Domain/OS:  cannot read kmem, so cannot use getproc.
    m68k|a88k)  cc $HSI_CF alloc.c -o alloc.e;;
    *)		cc $HSI_CF alloc.c getproc.c -o alloc.e;;
esac
chmod		4755 alloc.e
mv -f		alloc.e ../hlib
rm		alloc.o

case $MACH in

#   Domain/OS:  assembler is not a product, and can only be acquired
#   directly from Apollo R&D.  None of the other compilers act as front
#   ends.  File MUST be named ".asm", not ".s".

    m68k)	/usr/apollo/bin/asm ../as/zsvjmp.asm -nl -db; \
		mv -f zsvjmp.bin zsvjmp.o;;
    a88k)	;;
    *)		cc -c $HSI_CF	../as/zsvjmp.s;;
esac

cc -c $HSI_CF	[b-z]*.c
ar rv		libos.a *.o; ar dv libos.a zmain.o; rm *.o

case $MACH in

#   Domain/OS:  For m68k we have to use zdojmp_ entry point in zsvjmp.asm
#   rather than the C version (see notes.apollo.v29), so we remove zdojmp.o
#   from libos.a.  For a88k we use modified zdojmp.c.
#   We also have to pass zzstrt.o, which for Apollo has a reference to 
#   the Mem common, through /com/bind in order to align Mem at higher than
#   the default 4-byte boundary.

    m68k)	ar dv libos.a zdojmp.o;
esac
case $MACH in
    m68k|a88k)	ar xv libos.a zzstrt.o; \
		/com/bind zzstrt.o -align mem_ page -b zzstrt.bin; \
		mv -f zzstrt.bin zzstrt.o; \
		ar rv libos.a zzstrt.o;;
esac

case $USE_RANLIB in
    yes)	ranlib libos.a;;
esac
mv -f		libos.a ../bin

