# Make the Subset Preprocessor language (SPP) compiler.

echo "----------------------- XC  ----------------------------"
cc $HSI_CF	xc.c $HSI_LIBS -o xc.e
mv -f		xc.e ../../hlib

echo "----------------------- XPP ----------------------------"
(cd xpp; sh -x mkpkg.sh)
echo "----------------------- RPP ----------------------------"
(cd rpp; sh -x mkpkg.sh)
