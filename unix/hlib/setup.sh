# Allow a previously defined $iraf to be used.
d_iraf="/iraf/iraf/"
if [ -z "$iraf" ]; then
    if [ -r ${HOME}/.iraf/irafroot ] ; then
	export iraf=$(cat /etc/iraf/irafroot)
    elif [ -r /etc/iraf/irafroot ] ; then
	export iraf=$(cat /etc/iraf/irafroot)
    else
	export iraf="$d_iraf"
    fi
fi

# Allow a previously defined $IRAFARCH to be used.
if [ -z "$IRAFARCH" ]; then
    export IRAFARCH=$("$iraf/unix/hlib/irafarch.sh" -actual)
fi

export PATH=$HOME/.iraf/bin:${PATH}

# The world'd most obvious alias ....
alias iraf="xgterm -e cl &"

