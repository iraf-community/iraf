#!/bin/bash

# Allow a previously defined $iraf to be used.
if [ -n $iraf ]; then
    export iraf=/opt/iraf/iraf/
fi

# Allow a previously defined $IRAFARCH to be used.
if [ -n $IRAFARCH ]; then
    if test -f ${iraf}/unix/hlib/irafarch.sh; then
        export IRAFARCH=`${iraf}/unix/hlib/irafarch.sh -actual`
    fi
fi

# Define a minimal IRAF runtime environment.
export  hostid=unix
export  host=${iraf}/unix/
export  hlib=${iraf}/unix/hlib/
export  hbin=${iraf}/unix/bin.${IRAFARCH}/
export  tmp=/tmp/

# Uncomment to define a full IRAF development environment.
if test -f $iraf/unix/hlib/irafuser.sh; then
    source $iraf/unix/hlib/irafuser.sh
fi

export PATH=$HOME/.iraf/bin:${PATH}

# The world's most obvious alias ....
alias iraf="xgterm -e cl &"

