# Make the first pass (XPP) of the SPP language compiler.

find xpp.l -newer lexyy.c -exec rm lexyy.c \;
if test -f lexyy.c; then\
    cc -c $HSI_CF lexyy.c;\
else\
    lex	xpp.l;\
    sed -f lex.sed lex.yy.c > lexyy.c;  rm lex.yy.c;\
    cc -c $HSI_CF lexyy.c;\
fi

cc -c $HSI_CF	xppmain.c xppcode.c decl.c
cc $HSI_CF	xppmain.o lexyy.o xppcode.o decl.o $HSI_LIBS -o xpp.e
mv -f		xpp.e ../../../hlib
rm		*.o
