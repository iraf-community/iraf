# Make the SGI translators and install them in hlib.

cc -c $HSI_CF	sgidispatch.c
cc $HSI_CF	sgidispatch.o ../../hlib/libos.a -o sgidispatch.e
mv -f		sgidispatch.e ../../hlib
rm		sgidispatch.o

cc -c $HSI_CF	sgi2uimp.c
cc $HSI_CF	sgi2uimp.o -o sgi2uimp.e
mv -f		sgi2uimp.e ../../hlib
rm		sgi2uimp.o

cc -c $HSI_CF	sgi2uapl.c
cc $HSI_CF	sgi2uapl.o -o sgi2uapl.e
mv -f		sgi2uapl.e ../../hlib
rm		sgi2uapl.o

cc -c $HSI_CF	sgi2uqms.c
cc $HSI_CF	sgi2uqms.o -o sgi2uqms.e
mv -f		sgi2uqms.e ../../hlib
rm		sgi2uqms.o

cc -c $HSI_CF	sgi2uptx.c
cc $HSI_CF	sgi2uptx.o -o sgi2uptx.e
mv -f		sgi2uptx.e ../../hlib
rm		sgi2uptx.o
