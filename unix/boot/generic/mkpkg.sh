# Bootstrap the generic preprocessor.  The -lln library is not used to avoid
# the enternal dependency.  The sed script is used to edit certain nonportable
# constructs in the LEX code, and the filename lex.yy.c is changed to lexyy.c
# for portability reasons.

find tok.l -newer lexyy.c -exec rm lexyy.c \;
if test -f lexyy.c; then\
    $CC -c $HSI_CF lexyy.c;\
else\
    lex	tok.l;\
    sed -f lex.sed lex.yy.c > lexyy.c;  rm lex.yy.c;\
    $CC -c $HSI_CF lexyy.c;\
fi

$CC -c $HSI_CF	generic.c chario.c yywrap.c
$CC $HSI_CF	generic.o lexyy.o chario.o yywrap.o $HSI_LIBS -o generic.E
mv -f		generic.E ../../hlib
rm		*.o
