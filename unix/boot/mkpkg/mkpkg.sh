# Bootstrap MKPKG.

$CC -c $HSI_CF	char.c fdcache.c fncache.c host.c main.c pkg.c scanlib.c\
		    sflist.c tok.c
$CC $HSI_CF	main.o char.o fdcache.o fncache.o host.o pkg.o scanlib.o\
		    sflist.o tok.o $HSI_LIBS\
		    -o mkpkg.E

mv -f mkpkg.E ../../hlib
rm *.o
