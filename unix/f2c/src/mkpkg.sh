#!/bin/sh
# Bootstrap the F2C compiler and libraries.

# Exit on error
set -e

make -f makefile.u
mv f2c ../../bin/f2c.e
rm *.o 
