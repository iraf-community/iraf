#!/bin/sh
# Bootstrap WTAR.

# Exit on error
set -e

$CC -c $HSI_CF	wtar.c
$CC $HSI_LF	wtar.o $HSI_LIBS -o wtar.e
mv		wtar.e ../../hlib
rm -f		wtar.o
