# Bootstrap the generic preprocessor.  The -lln library is not used to avoid
# the enternal dependency.  The sed script is used to edit certain nonportable
# constructs in the LEX code, and the filename lex.yy.c is changed to lexyy.c
# for portability reasons.

# lexyy.c cannot be re-created in the moment due to lex incompatibilities
# with modern lex implementations.
#find tok.l -newer lexyy.c -exec rm lexyy.c

if test -f lexyy.c; then\
    $CC -c $HSI_CF -w lexyy.c;\
else\
    lex	tok.l;\
    sed -f lex.sed lex.yy.c > lexyy.c;  rm lex.yy.c;\
    $CC -c $HSI_CF -w lexyy.c;\
fi

$CC -c $HSI_CF	generic.c chario.c yywrap.c
$CC $HSI_LF	generic.o lexyy.o chario.o yywrap.o $HSI_LIBS -o generic.e
mv -f		generic.e ../../hlib
rm		*.o
