# Bootstrap the F2C compiler and libraries.

echo "----------------------- F2C ---------------------------"
(cd src;    sh -x mkpkg.sh)
echo "----------------------- LIBF2C ------------------------"
(cd libf2c; sh -x mkpkg.sh)
