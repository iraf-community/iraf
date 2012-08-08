# Bootstrap EDSYM (required by XC).

echo "make edsym.e"
if [ $OSVERSION = 4 ] ; then
$CC -c $HSI_CF edsym-sos4.c
$CC $HSI_LFLAGS edsym-sos4.o $HSI_LIBS -o edsym.e ;
else
$CC -c $HSI_CF edsym-ssol.c
$CC $HSI_LFLAGS edsym-ssol.o $HSI_LIBS -o edsym.e ;
fi
mv -f edsym.e ../hlib
rm -f edsym.o
