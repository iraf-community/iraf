#!/bin/sh
# Patch f2c and libf2c for the changes done in the IRAF version

set -e

################### F2C ######################
# Base: "src" subdir of f2c-20200916.tar.gz, f.e. from the Debian sources
# Remove files not needed here
rm -f src/f2c.h src/readme src/tokdefs.h src/xsum0.out src/index.html src/mkfile.plan9 

################# LIBF2C ####################
# Base: "src" subdir of libf2c2-20140711.tar.gz, f.e. from the Debian sources
# Remove files not needed here
rm -f libf2c/xsum0.out libf2c/libf2c/mkfile.plan9 libf2c/comptry.bat  libf2c/scomptry.bat


# Patch f2c and libf2c
for p in patches/*.patch; do
    echo $p
    patch -p3 < $p
done
