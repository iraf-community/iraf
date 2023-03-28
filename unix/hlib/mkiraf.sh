#!/bin/sh
#
#  MKIRAF -- Setup the IRAF environment for a user.  Should be called from the
#  directory from which the user will thereafter type "cl" to start a session.
#
#  Usage:
#
#	% mkiraf [--default] [--init] [--noinit] [--copy] [--quiet]
#
#  Where
#	-d,--default		Create default login dir
#	-i,--init		Initialize the uparm directory
#	-n,--noinit		Do not nitialize the uparm directory
#	-c,--copy		Copy login.cl file
#	-q,--quiet		Suppress output
#
#  Use of the -i or -n options will suppress the corresponding prompt
#  for input.


# Initialize the script variables.
uparm_init="ask"
quiet=""
def=""
copy=""
imdir="${HOME}/.iraf/imdir/"
cachedir="${HOME}/.iraf/cache/"

# Path edited by the install script.
iraf="/iraf/iraf/"

# Parse the command-line options.
for i in "$@"; do
    case $i in
	-d|--default)			# Create default login dir
            def="default"
            quiet="quiet"
	    echo ""
    	    ;;
	-i|--init)			# Initialize uparm directory
            uparm_init="yes"
    	    ;;
	-n|--noinit)			# Don't initialize uparm directory
            uparm_init="no"
    	    ;;
	-c|--copy)                      # Copy login.cl file
	    copy="yes"
	    ;;
	-q|--quiet)			# Suppress output
            quiet="quiet"
    	    ;;
	*)
            echo "Error: unknown option '$i'"
	    exit 1
    	    ;;
    esac
done

# With --default, chdir to the default directory to create uparm and
# login.cl there
if [ "$def" ]; then
    cd "${HOME}/.iraf/"
else
    # Protect against running mkiraf in an iraf system directory.
    irafdir=$(cd "$iraf" ; pwd)
    if (pwd | grep -q "^$irafdir") && ! (pwd | grep -q iraf/local); then
	echo "Error: current directory is not an iraf user login directory"
	exit 1
    fi
fi

# Create imdir and cache dir if not already there
mkdir -p "${imdir}" "${cachedir}"

# Make an empty "uparm" (user parameter) directory.
if [ ! -e uparm ]; then
    if [ ! "$quiet" ]; then
        echo '-- creating a new uparm directory'
    fi
    mkdir uparm
elif [ ! -d uparm ]; then
    echo "Error: a file uparm exists"
    exit 1
else
    if [ "$uparm_init" = "ask" ] ; then
	if [ ! "$quiet" ] ; then
            printf 'Initialize uparm? (y|n): '
            read yesno
	else
	    yesno="yes"
	fi
	if [ "$yesno" = "y" ] || [ "$yesno" = "yes" ]; then
	    if [ ! "$quiet" ]; then
		echo '-- initializing uparm'
	    fi
	    rm -rf uparm
	    mkdir uparm
	fi
    elif [ "$uparm_init" = "yes" ]; then
	if [ ! "$quiet" ]; then
	    echo '-- initializing uparm'
	fi
	rm -rf uparm
	mkdir uparm
    fi
fi


# Create local login.cl
if [ "$copy" ]; then
    if [ -e login.cl ]; then
	mv -f login.cl login.cl.OLD
    fi
    cp -f "${iraf}unix/hlib/login.cl" login.cl
fi

if [ ! "$def" ]; then
    echo 'A new LOGIN.CL file has been created in the current directory.'
    echo 'You may wish to review and edit this file to change the defaults.'
fi
