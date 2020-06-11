#!/bin/sh
# Bootstrap the generic preprocessor.  The -lln library is not used to avoid
# the enternal dependency.  The filename lex.yy.c is changed to lexyy.c
# for portability reasons.

# Exit on error
set -e

flex -o lexyy.c tok.l

$CC -c $HSI_CF	generic.c yywrap.c lexyy.c
$CC $HSI_LF	generic.o lexyy.o yywrap.o $HSI_LIBS -o generic.e
mv -f		generic.e ../../hlib
rm		*.o
