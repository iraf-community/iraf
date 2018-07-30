#!/bin/sh
# Bootstrap the bootstrap utilities.  The logical directory hlib$ should be
# defined for the cshell when this is run.

# Exit on error
set -e

echo "----------------------- BOOTLIB ------------------------"
(cd bootlib; sh -x mkpkg.sh)
echo "----------------------- GENERIC ------------------------"
(cd generic; sh -x mkpkg.sh)
echo "----------------------- MKPKG --------------------------"
(cd mkpkg;   sh -x mkpkg.sh)
echo "----------------------- RMBIN  -------------------------"
(cd rmbin;   sh -x mkpkg.sh)
echo "----------------------- RMFILES  -----------------------"
(cd rmfiles; sh -x mkpkg.sh)
echo "----------------------- RTAR  --------------------------"
(cd rtar;    sh -x mkpkg.sh)
echo "----------------------- WTAR --------------------------"
(cd wtar;    sh -x mkpkg.sh)
echo "----------------------- SPP ----------------------------"
(cd spp;     sh -x mkpkg.sh)
echo "----------------------- XYACC --------------------------"
(cd xyacc;   sh -x mkpkg.sh)
