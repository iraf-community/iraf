# Bootstrap EDSYM (required by XC).

echo "make edsym.e"
$CC -c $HSI_CF edsym.c
$CC $HSI_LFLAGS edsym.o $HSI_LIBS -o edsym.e
mv -f edsym.e ../hlib
rm -f edsym.o
