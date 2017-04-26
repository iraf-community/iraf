#!/bin/bash
#
#  MKIRAF -- Setup the IRAF environment for a user.  Should be called from the
#  directory from which the user will thereafter type "cl" to start a session.
#
#  Usage:
#
#	% mkiraf [--term=<term>] [--init] [--noinit] [--quiet]
#
#  Where
#	-t,--term=<term>	Set the default terminal type
#	-i,--init		Initialize the uparm directory
#	-n,--noinit		Do not nitialize the uparm directory
#	-q,--quiet		Suppress output
#
#  Use of the -t, -i, or -n options will suppress the corresponding prompts
#  for input.


				# Initialize the script variables.
myterm="none" 			
uparm_init=-1
quiet=0
def=0
defterm="xgterm"

				# Paths edited by the install script.
iraf="/iraf/iraf/" 		
imdir="/iraf/imdir/"
cachedir="/iraf/cache/"


# ------------- (end of site dependent definitions) ------------------------

# The following kludge is for Solaris, which doesn't have whoami.
if [ "$USER" = "" ]; then
    USER=`whoami`
fi


# Parse the command-line options.
for i in "$@"
do
  case $i in
    -t=*|--term=*)			# Set the default terminal type
        myterm=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
    	;;
    -d|--default)			# Create default login dir
        def=1
        quiet=1
	echo ""
    	;;
    -i|--init)				# Initialize uparm directory
        uparm_init=1
    	;;
    -n|--noinit)			# Don't initialize uparm directory
        uparm_init=0
    	;;
    -q|--quiet)				# Suppress output
        quiet=1
    	;;
    *)
        /bin/echo "Error: unknown option '$i'"
	exit 1
    	;;
  esac
done



# Protect against running mkiraf in an iraf system directory.
irafdir=`cd $iraf ; pwd`
if [ ! "`pwd | grep $irafdir`" = "" ]; then
    if [ "`pwd | grep iraf/local`" = "" ]; then
	/bin/echo "Error: current directory is not an iraf user login directory"
	exit 1
    fi
fi

if (( $def == 1 )); then
    imdir=`echo $HOME`"/.iraf/imdir/"
    cachedir=`echo $HOME`"/.iraf/cache/"
    #myterm="xgterm"
    cd $HOME
    if [ ! -e .iraf ]; then
	mkdir $HOME/.iraf
    fi
    cd $HOME/.iraf
    if [ ! -e bin ]; then
        mkdir bin
    fi
    if [ ! -e imdir ]; then
        mkdir imdir
    fi
    if [ ! -e cache ]; then
        mkdir cache
    fi
    cp $iraf/unix/hlib/setup.*sh .
fi


# Make an empty "uparm" (user parameter) directory.
if [ ! -e uparm ]; then
    if (( quiet<1 )); then
      if (( $def == 0 )); then
        /bin/echo '-- creating a new uparm directory'
      fi
    fi
    mkdir uparm
elif [ ! -d uparm ]; then
    /bin/echo "Error: a file uparm exists"
    exit 1
else
    if (( uparm_init<0 )) ; then
      if (( quiet<1 )) ; then
        /bin/echo -n 'Initialize uparm? (y|n): '
        read yesno
      else
	yesno="yes"
      fi
      if [ "$yesno" = "y" -o "$yesno" = "yes" ]; then
	if (( quiet<1 )); then
	    /bin/echo '-- initializing uparm'
	fi
	/bin/rm -rf uparm
	mkdir uparm
      fi
    elif (( uparm_init==1 )); then
	if (( quiet<1 )); then
	    /bin/echo '-- initializing uparm'
	fi
	/bin/rm -rf uparm
	mkdir uparm
    fi
fi

# Edit the login.cl file, setting the user's home directory, default image
# directory, and terminal.

if [ "$myterm" == "none" ]; then
    /bin/echo "Terminal types: xgterm,xtermjh,xterm,etc."
    /bin/echo -n 'Enter terminal type ('$defterm'): '
    read myterm
    if [ "$myterm" == "" ]; then
	myterm=$defterm
    fi
fi

# Initialize the 'imdir' and 'cachedir' paths.
IDIR="${imdir}$USER"
if [ -d $imdir ]; then
    mkdir -p $IDIR &> /dev/null
fi
if [ ! -d $IDIR -o ! -w $IDIR ]; then
    IDIR="HDR$"
fi

CDIR="${cachedir}$USER"
if [ -d $cachedir ]; then
    mkdir -p $CDIR &> /dev/null
fi
if [ ! -d $CDIR -o ! -w $CDIR ]; then
    CDIR="/tmp"
fi


# Back up the old login.cl file.
if [ -e login.cl ]; then
    mv -f login.cl login.cl.OLD
fi

# Create the path editing script.
_sed() {
    /bin/echo $1	| sed -e "s;.*;s+U_TERM+&+;"
    pwd			| sed -e "s;.*;s+U_HOME+&/+;"
    pwd			| sed -e "s;.*;s+U_UPARM+&/uparm/+;"
    /bin/echo $IDIR	| sed -e "s;.*;s+U_IMDIR+&/+;"
    /bin/echo $CDIR	| sed -e "s;.*;s+U_CACHEDIR+&/+;"
    /bin/echo $USER	| sed -e "s;.*;s+U_USER+&+;"
}

sed "`_sed $myterm`" < ${iraf}/unix/hlib/login.cl > login.cl

if (( $def == 0 )); then
 if (( quiet<1 )) ; then
  /bin/echo 'A new LOGIN.CL file has been created in the current directory.'
  /bin/echo 'You may wish to review and edit this file to change the defaults.'
 fi
fi
