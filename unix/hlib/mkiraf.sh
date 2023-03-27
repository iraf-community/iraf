#!/bin/sh
#
#  MKIRAF -- Setup the IRAF environment for a user.  Should be called from the
#  directory from which the user will thereafter type "cl" to start a session.
#
#  Usage:
#
#	% mkiraf [--init] [--noinit] [--quiet]
#
#  Where
#	-i,--init		Initialize the uparm directory
#	-n,--noinit		Do not nitialize the uparm directory
#	-q,--quiet		Suppress output
#
#  Use of the -i or -n options will suppress the corresponding prompts
#  for input.


# Initialize the script variables.
uparm_init=-1
quiet=0
def=0

imdir="${HOME}/.iraf/imdir/"
cachedir="${HOME}/.iraf/cache/"

# Paths edited by the install script.
iraf="/home/ole/Projects/2015/upstream/iraf/iraf/"



# ------------- (end of site dependent definitions) ------------------------

# The following kludge is for Solaris, which doesn't have whoami.
if [ "$USER" = "" ]; then
    USER=$(whoami)
fi


# Parse the command-line options.
for i in "$@"
do
  case $i in
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
        echo "Error: unknown option '$i'"
	exit 1
    	;;
  esac
done



# Protect against running mkiraf in an iraf system directory.
irafdir=$(cd "$iraf" ; pwd)
if [ ! "$(pwd | grep "$irafdir")" = "" ]; then
    if [ "$(pwd | grep iraf/local)" = "" ]; then
	echo "Error: current directory is not an iraf user login directory"
	exit 1
    fi
fi

if [ "$def" = 1 ]; then
    imdir="$HOME/.iraf/imdir/"
    cachedir="$HOME/.iraf/cache/"
    cd "$HOME"
    if [ ! -e .iraf ]; then
	mkdir "$HOME/.iraf"
    fi
    cd "$HOME/.iraf"
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
    if [ "$quiet" -lt 1 ]; then
      if [ "$def" = 0 ]; then
        echo '-- creating a new uparm directory'
      fi
    fi
    mkdir uparm
elif [ ! -d uparm ]; then
    echo "Error: a file uparm exists"
    exit 1
else
    if [ "$uparm_init" -lt 0 ] ; then
      if [ "$quiet"  -lt 1 ] ; then
        printf 'Initialize uparm? (y|n): '
        read yesno
      else
	yesno="yes"
      fi
      if [ "$yesno" = "y" ] || [ "$yesno" = "yes" ]; then
	if [ "$quiet" -lt 1 ]; then
	    echo '-- initializing uparm'
	fi
	rm -rf uparm
	mkdir uparm
      fi
    elif [ "$uparm_init" = 1 ]; then
	if [ "$quiet" -lt 1 ]; then
	    echo '-- initializing uparm'
	fi
	rm -rf uparm
	mkdir uparm
    fi
fi

# Edit the login.cl file, setting the user's home directory, and
# default image directory

# Initialize the 'imdir' and 'cachedir' paths.
IDIR="${imdir}$USER"
if [ -d "$imdir" ]; then
    mkdir -p "$IDIR"
fi
if [ ! -d "$IDIR" ] || [ ! -w "$IDIR" ]; then
    IDIR="HDR$"
fi

CDIR="${cachedir}$USER"
if [ -d "$cachedir" ]; then
    mkdir -p "$CDIR"
fi
if [ ! -d "$CDIR" ] || [ ! -w "$CDIR" ]; then
    CDIR="/tmp"
fi


# Back up the old login.cl file.
if [ -e login.cl ]; then
    mv -f login.cl login.cl.OLD
fi

# Create the path editing script.
_sed() {
    pwd			| sed -e "s;.*;s+U_HOME+&/+;"
    pwd			| sed -e "s;.*;s+U_UPARM+&/uparm/+;"
    echo "$IDIR"	| sed -e "s;.*;s+U_IMDIR+&/+;"
    echo "$CDIR"	| sed -e "s;.*;s+U_CACHEDIR+&/+;"
    echo "$USER"	| sed -e "s;.*;s+U_USER+&+;"
}

sed "$(_sed)" < "${iraf}/unix/hlib/login.cl" > login.cl

if [ $def = 0 ]; then
 if [ $quiet -lt 1 ] ; then
  echo 'A new LOGIN.CL file has been created in the current directory.'
  echo 'You may wish to review and edit this file to change the defaults.'
 fi
fi
