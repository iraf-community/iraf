# Set the HSI architecture.

# Set the link 'as'.
if test -h as; then rm -rf as; fi
    ln -s as.$MACH as

# Ditto for 'bin'.
if test -h bin; then rm -rf bin; fi
    ln -s bin.$MACH bin
