#!/bin/sh
# MKIRAF -- Setup the IRAF environment for a user.  Should be called from the
# directory from which the user will thereafter type "cl" to start a session.

PREFIX=
HOSTID=

iraf="$PREFIX/iraf/iraf/"
imdir="$PREFIX/iraf/imdirs/"

host="${iraf}${HOSTID}/"
hconfig="${host}config/"

ttymsg="Terminal types: xgterm,xterm,gterm,vt640,vt100,etc."

# ------------- (end of site dependent definitions) ------------------------

# The following kludge is for Solaris, which doesn't have whoami.
if [ "$USER" = "" ]; then
    USER=`whoami`
fi

# Protect against running mkiraf in an iraf system directory.
irafdir=`cd $iraf ; pwd`
if [ ! "`pwd | grep $irafdir`" = "" ]; then
    if [ "`pwd | grep iraf/local`" = "" ]; then
	echo "Error: current directory is not an iraf user login directory"
	exit 1
    fi
fi

# Make an empty "uparm" (user parameter) directory.
if [ ! -e uparm ]; then
    echo '-- creating a new uparm directory'
    mkdir uparm
elif [ ! -d uparm ]; then
    echo "Error: a file uparm exists"
    exit 1
else
    echo -n 'Initialize uparm? (y|n): '
    read yesno
    if [ "$yesno" = "y" -o "$yesno" = "yes" ]; then
	echo '-- initializing uparm'
	rm -rf uparm
	mkdir uparm
    fi
fi

if [ -e login.cl ]; then
    mv -f login.cl login.cl.OLD
fi


# Edit the login.cl file, setting the user's home directory, default image
# directory, and terminal.

echo $ttymsg
echo -n 'Enter terminal type: '
read ans


IDIR="${imdir}$USER"
if [ -d $imdir ]; then
    mkdir -p $IDIR &> /dev/null
fi
if [ ! -d $IDIR -o ! -w $IDIR ]; then
    IDIR=HDR$
fi

_sed() {
    echo $1	| sed -e "s;.*;s+U_TERM+&+;"
    pwd		| sed -e "s;.*;s+U_HOME+&/+;"
    pwd		| sed -e "s;.*;s+U_UPARM+&/uparm/+;"
    echo $IDIR	| sed -e "s;.*;s+U_IMDIR+&/+;"
    echo $USER	| sed -e "s;.*;s+U_USER+&+;"
}

sed "`_sed $ans`" < ${hconfig}login.cl > login.cl

echo 'A new LOGIN.CL file has been created in the current directory.'
echo 'You may wish to review and edit this file to change the defaults.'
