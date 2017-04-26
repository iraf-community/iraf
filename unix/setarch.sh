# Set the HSI architecture.

# Set the link 'as'.
if [ "`ls -d as`" = "as" ]; then rm -rf as; fi
    ln -s as.$MACH as

# Ditto for 'bin'.
if [ "`ls -d bin`" = "bin" ]; then rm -rf bin; fi
    ln -s bin.$MACH bin
