# Make the Subset Preprocessor language (SPP) compiler.

echo "----------------------- XC  ----------------------------"
$CC $HSI_CF	xc.c $HSI_LIBS -o xc.E
mv -f		xc.E ../../hlib

echo "----------------------- XPP ----------------------------"
(cd xpp; sh -x mkpkg.sh)
echo "----------------------- RPP ----------------------------"
(cd rpp; sh -x mkpkg.sh)
