# Bootstrap EDSYM (required by XC).

echo "make edsym.e"
$CC $HSI_CF edsym.c -o edsym.e
mv -f edsym.e ../hlib
