#!/bin/sh
# Bootstrap the F2C compiler and libraries.

# Exit on error
set -e

make -f makefile.u
mv libf2c.a ../../bin/
rm *.[aeo] 
