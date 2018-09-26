#!/bin/sh
# Bootstrap MKPKG.

# Exit on error
set -e

$CC -c $HSI_CF	char.c fdcache.c fncache.c host.c main.c pkg.c scanlib.c\
		    sflist.c tok.c
$CC $HSI_LF	main.o char.o fdcache.o fncache.o host.o pkg.o scanlib.o\
		    sflist.o tok.o $HSI_LIBS -o mkpkg.e

mv -f mkpkg.e ../../hlib
rm *.o
