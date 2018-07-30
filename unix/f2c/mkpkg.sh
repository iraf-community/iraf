#!/bin/sh
# Bootstrap the F2C compiler and libraries.

# Exit on error
set -e

echo "----------------------- F2C ---------------------------"
(cd src;    sh -x mkpkg.sh)
echo "----------------------- LIBF2C ------------------------"
(cd libf2c; sh -x mkpkg.sh)
