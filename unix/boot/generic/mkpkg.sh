# Bootstrap the generic preprocessor.  The -lln library is not used to avoid
# the enternal dependency.  The sed script is used to edit certain nonportable
# constructs in the LEX code, and the filename lex.yy.c is changed to lexyy.c
# for portability reasons.

find tok.l -newer lexyy.c -exec rm lexyy.c \;
if test -f lexyy.c; then\
    cc -c $HSI_CF lexyy.c;\
else\
    lex	tok.l;\
    sed -f lex.sed lex.yy.c > lexyy.c;  rm lex.yy.c;\
    cc -c $HSI_CF lexyy.c;\
fi

cc -c $HSI_CF	generic.c chario.c yywrap.c
cc $HSI_CF	generic.o lexyy.o chario.o yywrap.o $HSI_LIBS -o generic.e
mv -f		generic.e ../../hlib
rm		*.o
