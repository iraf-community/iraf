# Bootstrap the generic preprocessor.  The -lln library is not used to avoid
# the enternal dependency.  The sed script is used to edit certain nonportable
# constructs in the LEX code, and the filename lex.yy.c is changed to lexyy.c
# for portability reasons.

flex -o lexyy.c tok.l
$CC -c $HSI_CF	generic.c lexyy.c yywrap.c
$CC $HSI_LF	generic.o lexyy.o yywrap.o $HSI_LIBS -o generic.e

mv -f		generic.e ../../hlib
rm		*.o
