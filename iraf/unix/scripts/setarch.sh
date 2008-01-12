# Set the HSI architecture.

# Set the link 'as'.
if [ "`ls -d as`" = "as" ]; then rm -rf as; fi
    ln -s as.$MACH as

# Ditto for 'bin'.
if [ "`ls -d bin`" = "bin" ]; then rm -rf bin; fi
    ln -s bin.$MACH bin

mkdir -p include

( cd include ; rm -rf f2c.h ; ln -s ../bin/f2c.h . )
( cd include ; rm -rf iraf.h ; ln -s ../hlib/libc/iraf.h . )
( cd include ; rm -rf iraf ; ln -s ../hlib/libc iraf )

( 
  cd hlib
  if [ "$MACH" = "x86_64-linux-generic" ]; then
      rm -f iraf.h mach.h
      ln -s iraf.x86_64.h iraf.h
      ln -s mach.x86_64.h mach.h
  else
      rm -f iraf.h mach.h
      ln -s iraf.generic.h iraf.h
      ln -s mach.generic.h mach.h
  fi
)
