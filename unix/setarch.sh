# Set the HSI architecture.

# Ditto for 'bin'.
if [ "`ls -d bin`" = "bin" ]; then rm -rf bin; fi
    ln -s bin.$MACH bin
