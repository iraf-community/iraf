#!/bin/bash

# Allow a previously defined $iraf to be used.
if [ -z "$iraf" ]; then
    export iraf=/iraf/iraf/
fi

# Allow a previously defined $IRAFARCH to be used.
if [ -n $IRAFARCH ]; then
    export IRAFARCH=`$iraf/unix/hlib/irafarch.sh -actual`
fi
source $iraf/unix/hlib/irafuser.sh

export PATH=$HOME/.iraf/bin:${PATH}

# The world'd most obvious alias ....
alias iraf="xgterm -e cl &"

