# Make the second pass (RPP) of the SPP language compiler.

echo "----------------------- RPPFOR -------------------------"
(cd rppfor;	sh -x mkpkg.sh)
echo "----------------------- RATLIBF ------------------------"
(cd ratlibf;	sh -x mkpkg.sh)
echo "----------------------- RATLIBC ------------------------"
(cd ratlibc;	sh -x mkpkg.sh)

$CC -c $HSI_CF	rpp.c
$CC $HSI_LF	rpp.o librpp.a libf.a libc.a $HSI_F77LIBS -o rpp.e
mv -f		rpp.e ../../../hlib
rm		*.[ao]
