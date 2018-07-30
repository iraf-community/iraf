#!/bin/sh
# Make and install the RMFILES utility.

# Exit on error
set -e

$CC -c $HSI_CF	rmfiles.c
$CC $HSI_LF	rmfiles.o $HSI_LIBS -o rmfiles.e
mv -f		rmfiles.e ../../hlib
rm		*.o
