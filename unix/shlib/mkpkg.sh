# Bootstrap EDSYM (required by XC).

echo "make edsym.e"
cc $HSI_CF edsym.c -o edsym.e
mv -f edsym.e ../hlib
