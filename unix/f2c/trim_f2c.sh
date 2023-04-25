#!/bin/sh
# Patch f2c and libf2c for the changes done in the IRAF version

set -e

################### F2C ######################
# Base: "src" subdir of f2c-20100827.tar.gz, f.e. from the Debian
# sources
# Remove files not needed here
rm -f src/f2c.h src/readme src/tokdefs.h src/xsum0.out src/index.html src/mkfile.plan9 


# Patch f2c and libf2c
for p in patches/*.patch; do
    echo $p
    patch -p3 < $p
done
