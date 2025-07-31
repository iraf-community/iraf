#!/bin/sh
#
#  MKPKG -- Make a package or the core system.

# Initialize the $iraf and environment.
function arch_init () {
    if test -f $HOME/.iraf/envinit ; then
        source $HOME/.iraf/envinit
    elif test -f $iraf/unix/hlib/envinit.sh ; then
        source $iraf/unix/hlib/envinit.sh
    fi
}
arch_init

function arg_is_arch() {
    if [ "$1" == "macosx"  -o "$1" == "macintel" -o "$1" == "macos64" -o \
         "$1" == "linux64" -o "$1" == "linux" -o "$1" == "generic" ]; then
            is_arch=1
    else
            is_arch=0
    fi
}

# Get the architecture of the core system and ensure it's the same as
# what we're trying to build.

err=0
if [ -e "./bin" ]; then
    # In a package/system directory with a 'bin'.  Since we may be
    # reconfiguring the architecture, check the calling arguments first.

    last_arg=${!#}
    arg_is_arch "${last_arg}"
    if [ $is_arch -eq 1 ]; then
        # We are resetting the architecture so simply execute the binary.
        # The MKFLOAT in the mkpkg will warn about an invalid IRAFARCH, but
        # that isn't strictly required to reset a package arch.
        ${iraf}/unix/bin.${IRAFARCH}/mkpkg.e $*
        exit 0

    else
      # This is a build command with no architecture config so verify that
      # the current package is the same arch as the core system.

      p_arch=$(ls -l ./bin | cut -d '>' -f 2 | sed -e "s/ bin\.//g")
      if [ "${p_arch}" != "$IRAFARCH" ]; then
        err=1
      fi
    fi
fi


# Check the core system binary configuration and ensure it matches the current
# arch to be compiled.  For external packages that may link against NOAO
# libs like libasttools/libsmw the core arch needs to be defined to resolve
# the links correctly if pkglibs isn't set properly. This should probably be
# addressed in the mkpkg sources but we'll simply deal with the error here.

c_arch=$(ls -l ${iraf}/bin | cut -d '>' -f 2 | sed -e "s/ bin\.//g")
if [ "${c_arch}" != "$IRAFARCH" ]; then
    err=1
fi

if [ $err -eq 1 ]; then
    echo " "
    echo "ERROR: Architecture or environment configuration mismatch:"
    echo " "
    echo "       IRAFARCH environment:          "${IRAFARCH}
    echo "       Core IRAF arch configuration:  "${c_arch}
    if [ -e "./bin" ]; then
    echo "       Package arch configuration:    "${p_arch}
    echo " "
    fi
    exit 1
fi


# Execute the binary
${iraf}/unix/bin.${IRAFARCH}/mkpkg.e $*

/bin/rm -f bin.${IRAFARCH}/bin.${IRAFARCH}
