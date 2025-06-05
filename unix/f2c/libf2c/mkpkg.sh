# Bootstrap the F2C compiler and libraries.

make -f makefile.u
mv libf2c.a ../../bin/
rm *.[aeo] 
