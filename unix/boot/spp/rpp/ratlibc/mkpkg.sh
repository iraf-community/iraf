# Host system interface for the RPP program.

$CC -c $HSI_CF	cant.c close.c endst.c getarg.c getlin.c initst.c open.c\
		putch.c putlin.c r4tocstr.c remark.c

ar rv		libratc.a *.o
$RANLIB		libratc.a
mv -f		libratc.a ..
rm		*.o
