# Host system interface for the RPP program.

cc -c $HSI_CF	cant.c close.c endst.c getarg.c getlin.c initst.c open.c\
		putch.c putlin.c r4tocstr.c remark.c

ar rv		libc.a *.o
ranlib		libc.a
mv -f		libc.a ..
rm		*.o
