#!/bin/bash

# Allow a previously defined $iraf to be used.
if [ -n $iraf ]; then
    export iraf=/opt/iraf/iraf/
fi

# Allow a previously defined $IRAFARCH to be used.
if [ -n $IRAFARCH ]; then
    if test -f $iraf/unix/hlib/irafarch.sh; then
        export IRAFARCH=`$iraf/unix/hlib/irafarch.sh -actual`
    fi
fi
if test -f $iraf/unix/hlib/irafuser.sh; then
    source $iraf/unix/hlib/irafuser.sh
fi

export PATH=$HOME/.iraf/bin:${PATH}

# The world's most obvious alias ....
alias iraf="xgterm -e cl &"

