#!/bin/sh
# GDEV -- Host dependent graphics device drivers.

# Exit on error
set -e

(cd sgidev;  sh -x mkpkg.sh)
