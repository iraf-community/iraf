#!/bin/sh

# mkiraf -f means:
#	don't ask questions
#	do purge uparm

if [ "$1" = "-f" ]
then
	force=y
	shift
else
	force=n
fi

# apparently the expectation is that you might edit these after you install iraf:
imdir="/tmp"
ttymsg="Terminal types: xgterm,xterm,gterm,vt640,vt100,etc."

# ------------- (end of site dependent definitions) ------------------------

# note use of /bin/echo in place of echo because the shell built-in echo
# may not understand -n

# find our user name if we don't already know
if [ -z "$?USER" ]
then
    USER=`id | tr '()' '  ' | awk '{ print $2 }'`
fi

# where are we making this
irafhome=`pwd`/

# Make an empty "uparm" (user parameter) directory.
case "$force"
in
n)
	if [ ! -d uparm ]
	then
		echo '-- creating a new uparm directory'
		mkdir uparm
	else
		/bin/echo -n 'Initialize uparm? (y|n): '
		read ans

		case "$ans"
		in
		y|yes)
			echo '-- initializing uparm'
			rm -rf uparm
			mkdir uparm
			;;
		esac
	fi
	;;
y)
	rm -rf uparm
	mkdir uparm
	;;
esac


# preserve old login.cl
if [ -e login.cl ]
then
    mv -f login.cl login.cl.OLD
fi


# the terminal type is handled similarly to IRAF but without the command
# line option to specify it and with a default of $TERM
case "$force"
in
n)
	# find the terminal type
	echo $ttymsg
	/bin/echo -n "Enter terminal type [default $TERM]: "

	read termtype
	;;
y)
	termtype=''
	;;
esac

# default term type is whatever term is set to 

if [ "$termtype" = "" ]
then
	termtype=$TERM
fi

# strange handling of imdir preserved from mkiraf.csh

if [ -d $imdir ] && [ -w $imdir ]
then
	imdir=$imdir/$USER
	mkdir -p $imdir
else
	imdir='HDR$'
fi
imdir=$imdir/

# make a sed script that hacks up login.cl
# JT: the original cl.csh also substitutes U_UPARM below, but uparm is
# already set to home$uparm/ by default in login.cl.

cat > _sed << ARF
s?U_TERM?$termtype?g
s?U_HOME?$irafhome?g
s?U_IMDIR?$imdir?g
s?U_USER?$USER?g
ARF

# do it

sed -f _sed < $iraf/unix/hlib/login.cl > login.cl

rm _sed

case "$force"
in
n)
	echo 'A new LOGIN.CL file has been created in the current directory.'
	echo 'You may wish to review and edit this file to change the defaults.'
	echo 'You may also add additional customizations to loginuser.cl'
	;;
y)
	:
	;;
esac
